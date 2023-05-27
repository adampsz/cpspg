module SymbolSet = Set.Make (Automaton.Symbol)
module TermSet = Set.Make (Automaton.Terminal)
module NTermMap = Map.Make (Automaton.Nonterminal)

module NULLABLE (G : Types.Grammar) : sig
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
  include Types.Grammar

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

  let dummy = Term Terminal.dummy
  let first = List.to_seq (dummy :: G.symbols) |> Seq.map init |> SymbolMap.of_seq
  let first = loop first
  let first sym = SymbolMap.find_opt sym first |> Option.value ~default:TermSet.empty
end

(** Computation of FOLLOW sets *)
module FOLLOW (G : sig
  include Types.Grammar

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
  let dummy = Term Terminal.dummy
  let follow = List.to_seq (dummy :: G.symbols) |> Seq.map init |> SymbolMap.of_seq
  let follow = loop follow
  let follow sym = SymbolMap.find_opt sym follow |> Option.value ~default:TermSet.empty
end

module Run (S : Types.Settings) (G : Types.Grammar) : Types.Automaton = struct
  open Automaton

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

  let resolve_conflict id sym actions =
    let cmp l r =
      match l, r with
      | Some (_, l), Some (r, _) -> r - l
      | _, _ -> 0
    in
    let prec = (G.term sym).ti_prec in
    let filter f (p, a) = if a <> Shift && f (cmp prec p) 0 then Some a else None in
    (* Shift *)
    let shift = List.find_map (fun (_, a) -> if a = Shift then Some a else None) actions
    (* Reductions with precedence higher than shift *)
    and high = List.filter_map (filter ( > )) actions
    (* Reductions with precedence same as shift *)
    and equal = List.filter_map (filter ( = )) actions
    (* Reductions with precedence lower than shift *)
    and low = List.filter_map (filter ( < )) actions in
    match shift, high, equal, low with
    (* Single shift *)
    | Some shift, [], [], [] -> [ shift ]
    (* Single reducion *)
    | None, [ action ], [], [] -> [ action ]
    | None, [], [ action ], [] -> [ action ]
    | None, [], [], [ action ] -> [ action ]
    (* One reduction with precedence higher than shift *)
    | Some _, [ action ], [], [] -> [ action ]
    (* Many reductions with precedence lower than shift *)
    | Some shift, [], [], _ -> [ shift ]
    (* Conflict *)
    | _, _, _, _ ->
      let actions = List.map snd actions in
      S.report_conflict id sym actions;
      actions
  ;;

  let get_actions state =
    let actions = Hashtbl.create 16 in
    let register prec action sym =
      let xs = Hashtbl.find_opt actions sym |> Option.value ~default:[] in
      Hashtbl.replace actions sym ((prec, action) :: xs)
    in
    let iter_item lookahead i j item =
      if item.i_suffix = []
      then TermSet.iter (register item.i_prec (Reduce (i, j))) lookahead
    in
    let iter_group i group =
      let lookahead = lookahead group in
      List.iteri (iter_item lookahead i) group.g_items
    and iter_goto sym _ =
      match sym with
      | Term t -> register (G.term t).ti_prec Shift t
      | NTerm _ -> ()
    in
    List.iteri iter_group (state.s_kernel @ state.s_closure);
    SymbolMap.iter iter_goto state.s_goto;
    actions
  ;;

  let pack_actions id actions =
    let packed = Hashtbl.create 16 in
    let register sym action =
      let xs = Hashtbl.find_opt packed action |> Option.value ~default:TermSet.empty in
      Hashtbl.replace packed action (TermSet.add sym xs)
    in
    let iter sym actions = resolve_conflict id sym actions |> List.iter (register sym) in
    Hashtbl.iter iter actions;
    Hashtbl.to_seq packed |> Seq.map (fun (a, s) -> s, a) |> List.of_seq
  ;;

  let attach_actions id =
    let state = Hashtbl.find states id in
    let action = get_actions state |> pack_actions id in
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
