type grammar_type =
  | LR0
  | SLR
  | LR1
  | LALR

module type Settings = sig
  val kind : grammar_type
  val on_conflict : int -> Automaton.Terminal.t -> Automaton.action list -> unit
  val log : ('a, Format.formatter, unit) format -> 'a
end

module type Input = sig
  val grammar : Grammar.t
end

module IntMap = Map.Make (Int)
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
module SymbolSet = Set.Make (Automaton.Symbol)
module SymbolMap = Map.Make (Automaton.Symbol)
module TermSet = Set.Make (Automaton.Terminal)

module type Grammar = sig
  val header : string
  val term : Automaton.Terminal.t -> Automaton.term_info
  val nterm : Automaton.Nonterminal.t -> Automaton.nterm_info
  val group : Automaton.Nonterminal.t -> Automaton.group
  val symbols : Automaton.symbol list
  val actions : Automaton.semantic_action IntMap.t
end

module type Automaton = sig
  include Grammar

  val automaton : Automaton.t
end

module NULLABLE (G : Grammar) : sig
  val nullable : Automaton.symbol -> bool
end = struct
  open Automaton

  let item_nullable null item = List.for_all (fun s -> SymbolSet.mem s null) item.i_suffix

  let fold null = function
    | Term _ -> null
    | NTerm n when SymbolSet.mem (NTerm n) null -> null
    | NTerm n ->
      let group = G.group n in
      if List.exists (item_nullable null) group.g_items
      then SymbolSet.add (NTerm n) null
      else null
  ;;

  let rec loop null =
    let null' = List.fold_left fold null G.symbols in
    if SymbolSet.equal null null' then null else loop null'
  ;;

  let nullable = loop SymbolSet.empty
  let nullable sym = SymbolSet.mem sym nullable
end

(* Computation of FIRST sets *)
module FIRST (G : sig
  include Grammar

  val nullable : Automaton.symbol -> bool
end) : sig
  val first : Automaton.symbol -> TermSet.t
end = struct
  open Automaton

  let rec fold_item first x = function
    | [] -> x
    | sym :: symbols ->
      let x = TermSet.union x (SymbolMap.find sym first) in
      if G.nullable sym then fold_item first x symbols else x
  ;;

  let fold first = function
    | Term _ -> first
    | NTerm n ->
      let group = G.group n in
      let x = SymbolMap.find (NTerm n) first in
      let x = List.fold_left (fun x i -> fold_item first x i.i_suffix) x group.g_items in
      SymbolMap.add (NTerm n) x first
  ;;

  let rec loop first =
    let first' = List.fold_left fold first G.symbols in
    if SymbolMap.equal TermSet.equal first first' then first else loop first'
  ;;

  let init = function
    | NTerm _ as s -> s, TermSet.empty
    | Term t as s -> s, TermSet.singleton t
  ;;

  let first = List.to_seq G.symbols |> Seq.map init |> SymbolMap.of_seq
  let first = loop first
  let first sym = SymbolMap.find_opt sym first |> Option.value ~default:TermSet.empty
end

(** Computation of FOLLOW sets *)
module FOLLOW (G : sig
  include Grammar

  val first : Automaton.symbol -> TermSet.t
  val nullable : Automaton.symbol -> bool
end) : sig
  val follow : Automaton.symbol -> TermSet.t
end = struct
  open Automaton

  let first = G.first
  let nullable = G.nullable

  (** `fold_item (follow, def) symbols` adds new elements to `follow` set,
      based on production X → . β, where `symbols` is β and `def` is FOLLOW(X). *)
  let rec fold_item (follow, def) = function
    | [] -> follow, def
    | sym :: symbols ->
      let follow, acc = fold_item (follow, def) symbols in
      let x = SymbolMap.find sym follow in
      let follow = SymbolMap.add sym (TermSet.union x acc) follow in
      let y = first sym in
      follow, if nullable sym then TermSet.union acc y else y
  ;;

  let fold follow = function
    | Term _ -> follow
    | NTerm n ->
      let group = G.group n in
      let acc = follow, SymbolMap.find (NTerm group.g_symbol) follow in
      List.fold_left (fun a i -> fold_item a i.i_suffix) acc group.g_items |> fst
  ;;

  let rec loop follow =
    let follow' = List.fold_left fold follow G.symbols in
    if SymbolMap.equal TermSet.equal follow follow' then follow else loop follow'
  ;;

  let init s = s, TermSet.empty
  let follow = List.to_seq G.symbols |> Seq.map init |> SymbolMap.of_seq
  let follow = loop follow
  let follow sym = SymbolMap.find_opt sym follow |> Option.value ~default:TermSet.empty
end

module Run (S : Settings) (I : Input) : Automaton = struct
  open Automaton
  module TermMap = Map.Make (Terminal)
  module NTermMap = Map.Make (Nonterminal)

  module G : Grammar = struct
    let header = I.grammar.header
    let term = Hashtbl.create 16
    let nterm = Hashtbl.create 16

    (* Define terminals *)
    let _ =
      let iter_token ty name =
        let info = { ti_name = name; ti_ty = ty } in
        Hashtbl.replace term name (Hashtbl.length term |> Terminal.of_int, info)
      in
      let iter_decl = function
        | Grammar.DeclStart _ -> ()
        | Grammar.DeclToken (ty, ids) -> List.iter (iter_token ty) ids
      in
      List.iter iter_decl I.grammar.decls
    ;;

    (* Define non-terminals *)
    let _ =
      let iter_rule rule =
        let name = rule.Grammar.id in
        let info = { ni_name = name; ni_starting = false } in
        Hashtbl.replace nterm name (Hashtbl.length nterm |> Nonterminal.of_int, info)
      in
      List.iter iter_rule I.grammar.rules
    ;;

    (* Mark starting symbols *)
    let _ =
      let iter_start _ name =
        let id, info = Hashtbl.find nterm name in
        Hashtbl.replace nterm name (id, { info with ni_starting = true })
      in
      let iter_decl = function
        | Grammar.DeclStart (ty, ids) -> List.iter (iter_start ty) ids
        | Grammar.DeclToken _ -> ()
      in
      List.iter iter_decl I.grammar.decls
    ;;

    let symbols =
      let term = Hashtbl.to_seq_values term |> Seq.map (fun (t, _) -> Term t)
      and nterm = Hashtbl.to_seq_values nterm |> Seq.map (fun (n, _) -> NTerm n) in
      List.of_seq term @ List.of_seq nterm |> List.sort compare
    ;;

    let tr_symbol = function
      | Grammar.Term n -> Term (Hashtbl.find term n |> fst)
      | Grammar.NTerm n -> NTerm (Hashtbl.find nterm n |> fst)
    ;;

    let tr_symbols p = List.map (fun p -> tr_symbol p.Grammar.actual) p.Grammar.prod

    and tr_action p symbol index =
      let id = function
        | { Grammar.id = None; Grammar.actual = Grammar.NTerm name; _ } -> Some name
        | { Grammar.id; _ } -> id
      in
      let sa_args = List.map id p.Grammar.prod in
      { sa_symbol = symbol; sa_index = index; sa_args; sa_code = p.Grammar.action }
    ;;

    (** Create semantic action and item from given production `prod`, then register
       created action in `actions` and add item to `items`. *)
    let fold_prod symbol (actions, items, i) prod =
      let action, id = tr_action prod symbol i, IntMap.cardinal actions + 1 in
      let item = { i_suffix = tr_symbols prod; i_action = id } in
      IntMap.add id action actions, item :: items, i + 1
    ;;

    let compare_prod_length a b = List.length b.Grammar.prod - List.length a.Grammar.prod

    (** Create item group from given rule `rule`, while registering all its actions in `actions`,
       then add group to `groups`. *)
    let fold_rule (actions, groups) rule =
      let prods = List.sort compare_prod_length rule.Grammar.prods in
      let g_symbol = fst (Hashtbl.find nterm rule.Grammar.id)
      and g_lookahead = TermSet.empty in
      let actions, g_items, _ =
        List.fold_left (fold_prod g_symbol) (actions, [], 0) prods
      in
      let g_items = List.rev g_items in
      let group = { g_symbol; g_prefix = []; g_items; g_lookahead; g_starting = false } in
      actions, NTermMap.add group.g_symbol group groups
    ;;

    (** `actions` is a map from action id to semantic action, for all semantic actions
            defined in the grammar.
          `groups` is a map from nonterminal id to a item group in a form { X → ε · β1, …, βn },
            where X → β1, …, X → βn are producions from grammar *)
    let actions, groups =
      List.fold_left fold_rule (IntMap.empty, NTermMap.empty) I.grammar.rules
    ;;

    let term =
      let term = Hashtbl.to_seq_values term |> TermMap.of_seq in
      fun t -> TermMap.find t term
    ;;

    let nterm =
      let nterm = Hashtbl.to_seq_values nterm |> NTermMap.of_seq in
      fun n -> NTermMap.find n nterm
    ;;

    let group n = NTermMap.find n groups
  end

  let nullable =
    let module N = NULLABLE (G) in
    N.nullable
  ;;

  let first =
    let module F = FIRST (struct include G ;; let nullable = nullable ;; end) in
    F.first
    [@@ocamlformat "disable"]

  let follow =
    let module G = struct include G ;; let nullable = nullable ;; let first = first ;; end in
    let follow = lazy (let module F = FOLLOW (G) in F.follow) in
    fun sym -> Lazy.force follow sym
    [@@ocamlformat "disable"]

  (** Propagates lookaheads from `item` X -> α · Y β with lookahead `lookahead`
      onto `group` Y -> . γ1 ... γn. *)
  let propagate_lookahead lookahead suffix group =
    (* `get L α` returns sum of FIRST(αl) for every l ∈ L. *)
    let rec get la = function
      (* If α is empty, FIRST(αl) = l *)
      | [] -> la
      (* When sym is nullable, we need to add FIRST(symbols) to result. *)
      | sym :: symbols when nullable sym -> TermSet.union (first sym) (get la symbols)
      (* Otherwise, just return FIRST(sym). *)
      | sym :: _ -> first sym
    in
    let lookahead = get lookahead suffix in
    { group with g_lookahead = TermSet.union group.g_lookahead lookahead }
  ;;

  (* Attaches closure to given state. When `lookahead` is true also propagates LR(1) lookaheads. *)
  let closure ?(lookahead = false) state =
    (* Visits group item and adds its closure group to `extra` and `queue` if necessary. *)
    let fold_item la (extra, queue) = function
      | { i_suffix = NTerm sym :: suffix; _ } when lookahead ->
        let group = NTermMap.find_opt sym extra |> Option.value ~default:(G.group sym) in
        let group' = propagate_lookahead la suffix group in
        if NTermMap.mem sym extra && equal_groups group group'
        then extra, queue
        else NTermMap.add sym group' extra, group' :: queue
      (* When we see an item in a form X → α · Y β, where Y is a nonterminal which we haven't seen yet,
         we fetch an item group { Y → ε · γ1, … γn } and add it to both the closure and queue. *)
      | { i_suffix = NTerm sym :: _; _ } when NTermMap.mem sym extra = false ->
        let group = G.group sym in
        NTermMap.add sym group extra, group :: queue
      | _ -> extra, queue
    in
    (* Adds new groups to `extra` until no groups are left in `queue`. *)
    let rec loop extra = function
      | { g_items; g_lookahead; _ } :: que ->
        let extra, que = List.fold_left (fold_item g_lookahead) (extra, que) g_items in
        loop extra que
      | [] -> NTermMap.to_seq extra |> Seq.map snd |> List.of_seq
    in
    { state with s_closure = loop NTermMap.empty state.s_kernel }
  ;;

  let states = Hashtbl.create 128

  (** Visits freshly created state `state`, traverses its children
      and returns same state with but added children links. *)
  let rec visit_state state =
    let state = closure ~lookahead:(S.kind = LR1) state in
    let fold_goto shift symbol =
      match shift_state symbol state with
      | None -> shift
      | Some state' -> SymbolMap.add symbol (register_state state') shift
    in
    { state with s_goto = List.fold_left fold_goto SymbolMap.empty G.symbols }

  (** Registers state when necessery and returns its id. *)
  and register_state state =
    match Hashtbl.find_opt states state.s_kernel with
    | Some (id, _) -> id
    | None ->
      let id = Hashtbl.length states in
      Hashtbl.add states state.s_kernel (id, state);
      let state = visit_state state in
      Hashtbl.replace states state.s_kernel (id, state);
      id
  ;;

  let starting =
    let add_init_symbol = function
      | NTerm n when (G.nterm n).ni_starting ->
        let s_kernel = [ { (G.group n) with g_starting = true } ]
        and s_closure = [] in
        let state = { s_kernel; s_closure; s_goto = SymbolMap.empty; s_action = [] } in
        Some (n, register_state state)
      | NTerm _ | Term _ -> None
    in
    List.filter_map add_init_symbol G.symbols
  ;;

  let states = Hashtbl.to_seq_values states |> Hashtbl.of_seq

  module LR0 () = struct
    let f = function
      | NTerm _ -> None
      | Term t -> Some t
    ;;

    let lookahead = List.to_seq G.symbols |> Seq.filter_map f |> TermSet.of_seq
  end

  module LALR () = struct
    (* Attaches lookaheads to given state and propagates them onto its successors. *)
    let rec visit_state id state =
      let state = closure ~lookahead:true state in
      let iter sym id' = update_state id' (shift_state sym state |> Option.get) in
      Hashtbl.replace states id state;
      SymbolMap.iter iter state.s_goto

    (* Checks whether state kernel changed and visits it when necessary. *)
    and update_state id state' =
      let state = { (Hashtbl.find states id) with s_closure = [] } in
      let state' = merge_states state state' in
      if not (equal_states state state') then visit_state id state'
    ;;

    let iter i =
      let s = Hashtbl.find states i in
      if (List.hd s.s_kernel).g_starting then visit_state i s
    ;;

    (* Propagate LALR lookaheads for all states. *)
    let ids = Hashtbl.to_seq_keys states |> List.of_seq
    let _ = List.iter iter ids
  end

  let lookahead =
    match S.kind with
    (* LR0 grammars doesn't have any lookaheads *)
    | LR0 ->
      let module L = LR0 () in
      fun _ -> L.lookahead
    (* For LR1 grammar lookaheads were already computed during state generation. *)
    | LR1 -> fun g -> g.g_lookahead
    | SLR -> fun g -> follow (NTerm g.g_symbol)
    (* For LALR grammar, we need do attach lookaheads to already generated states. *)
    | LALR ->
      let module S = LALR () in
      fun g -> g.g_lookahead
  ;;

  let check_conflicts id actions =
    let map = Hashtbl.create 32 in
    let add_action action symbol =
      let actions = Hashtbl.find_opt map symbol |> Option.value ~default:[] in
      Hashtbl.replace map symbol (action :: actions)
    and check_conflict sym = function
      | [] | [ _ ] -> ()
      | actions -> S.on_conflict id sym actions
    in
    List.iter (fun (s, m) -> TermSet.iter (add_action m) s) actions;
    Hashtbl.iter check_conflict map
  ;;

  let determine_actions state =
    let add_reduce (i, actions) g =
      let f (j, actions) = function
        | { i_suffix = []; _ } -> j + 1, (lookahead g, Reduce (i, j)) :: actions
        | _ -> j + 1, actions
      in
      let _, actions = List.fold_left f (0, actions) g.g_items in
      i + 1, actions
    and add_shift actions s =
      let f = function
        | NTerm _, _ -> None
        | Term t, _ -> Some t
      in
      let lookahead = SymbolMap.to_seq s.s_goto |> Seq.filter_map f |> TermSet.of_seq in
      if TermSet.is_empty lookahead then actions else (lookahead, Shift) :: actions
    in
    let closure = state.s_kernel @ state.s_closure in
    List.fold_left add_reduce (0, add_shift [] state) closure |> snd
  ;;

  let attach_actions id =
    let state = Hashtbl.find states id in
    let action = determine_actions state in
    check_conflicts id action;
    Hashtbl.replace states id { state with s_action = action }
  ;;

  let ids = Hashtbl.to_seq_keys states |> List.of_seq
  let _ = List.iter attach_actions ids

  let automaton =
    { a_header = G.header
    ; a_actions = IntMap.to_seq G.actions |> IntMap.of_seq
    ; a_states = Hashtbl.to_seq states |> IntMap.of_seq
    ; a_starting = starting
    }
  ;;

  include G
end
