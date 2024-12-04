module IntMap = Map.Make (Int)
module TermMap = Map.Make (Automaton.Terminal)
module NTermMap = Map.Make (Automaton.Nonterminal)

module Run (S : Types.Settings) (A : Types.Ast) : Types.Grammar = struct
  open Automaton

  type prec_level =
    | PrecTerm of Terminal.t
    | PrecDummy of string

  let term = Hashtbl.create 16
  let nterm = Hashtbl.create 16
  let prec = Hashtbl.create 16

  let header =
    let get_code = function
      | Ast.DeclCode code -> Some code
      | _ -> None
    in
    List.filter_map get_code A.ast.decls
  ;;

  (* Define terminals *)
  let _ =
    let iter_token ty name =
      let info = { ti_name = name; ti_ty = ty; ti_prec = None } in
      if Hashtbl.mem term name.data
      then S.report_warn ~loc:name.loc "duplicate terminal symbol %s" name.data
      else Hashtbl.add term name.data (Hashtbl.length term |> Terminal.of_int, info)
    in
    let iter_decl = function
      | Ast.DeclToken (ty, ids) -> List.iter (iter_token ty) ids
      | _ -> ()
    in
    List.iter iter_decl A.ast.decls
  ;;

  (* Define non-terminals *)
  let _ =
    let iter_rule rule =
      let name = rule.Ast.id in
      let info = { ni_name = name; ni_starting = false } in
      if Hashtbl.mem nterm name.data
      then S.report_warn ~loc:name.loc "duplicate non-terminal symbol %s" name.data
      else Hashtbl.add nterm name.data (Hashtbl.length nterm |> Nonterminal.of_int, info)
    in
    List.iter iter_rule A.ast.rules
  ;;

  (* Mark starting symbols *)
  let _ =
    let iter_start name =
      match Hashtbl.find_opt nterm name.data with
      | Some (id, info) ->
        if info.ni_starting
        then S.report_warn ~loc:name.loc "symbol %s is already starting" name.data;
        Hashtbl.replace nterm name.data (id, { info with ni_starting = true })
      | None -> S.report_err ~loc:name.loc "unknown non-terminal symbol %s" name.data
    in
    let iter_decl = function
      | Ast.DeclStart (_, ids) -> List.iter iter_start ids
      | _ -> ()
    in
    List.iter iter_decl A.ast.decls
  ;;

  (* Define precedence levels *)
  let _ =
    let iter_sym p sym =
      let info, sym =
        match sym with
        | Ast.Term t -> Hashtbl.find_opt term t.data, t
        | Ast.NTerm n -> None, n
      in
      match info with
      | None ->
        if Hashtbl.mem prec (PrecDummy sym.data)
        then S.report_warn ~loc:sym.loc "duplicate precedence %s" sym.data;
        Hashtbl.replace prec (PrecDummy sym.data) p
      | Some (id, info) ->
        if Hashtbl.mem prec (PrecTerm id)
        then S.report_warn ~loc:sym.loc "duplicate precedence %s" sym.data;
        Hashtbl.replace prec (PrecTerm id) p;
        Hashtbl.replace term sym.data (id, { info with ti_prec = Some p })
    in
    let iter i = function
      | Ast.DeclLeft xs -> List.iter (iter_sym ((i * 2) + 1, i * 2)) xs
      | Ast.DeclRight xs -> List.iter (iter_sym (i * 2, (i * 2) + 1)) xs
      | Ast.DeclNonassoc xs -> List.iter (iter_sym (i * 2, i * 2)) xs
      | _ -> ()
    in
    List.iteri iter A.ast.decls
  ;;

  let symbols =
    let term = Hashtbl.to_seq_values term |> Seq.map (fun (t, _) -> Term t)
    and nterm = Hashtbl.to_seq_values nterm |> Seq.map (fun (n, _) -> NTerm n) in
    List.of_seq term @ List.of_seq nterm |> List.sort compare
  ;;

  let tr_term t =
    match Hashtbl.find_opt term t.data with
    | Some (id, _) -> Term id
    | None ->
      S.report_err ~loc:t.loc "unknown terminal symbol %s" t.data;
      Term Terminal.dummy

  and tr_nterm n =
    match Hashtbl.find_opt nterm n.data with
    | Some (id, _) -> NTerm id
    | None ->
      S.report_err ~loc:n.loc "unknown non-terminal symbol %s" n.data;
      Term Terminal.dummy
  ;;

  let tr_symbol = function
    | Ast.Term t -> tr_term t
    | Ast.NTerm n -> tr_nterm n
  ;;

  let tr_symbols p = List.map (fun p -> tr_symbol p.Ast.actual) p.Ast.prod

  let get_precedence symbols sym =
    let sym_prec sym =
      let id =
        match sym with
        | Ast.Term t -> t
        | Ast.NTerm n -> n
      in
      let prec =
        match Hashtbl.find_opt term id.data with
        | Some (id, _) -> Hashtbl.find_opt prec (PrecTerm id)
        | None -> Hashtbl.find_opt prec (PrecDummy id.data)
      in
      match prec with
      | Some prec -> Some prec
      | None ->
        S.report_err ~loc:id.loc "unknown precedence level %s" id.data;
        None
    in
    let fold sym acc =
      match sym, acc with
      | _, Some prec -> Some prec
      | Term t, None -> Hashtbl.find_opt prec (PrecTerm t)
      | NTerm _, None -> None
    in
    List.fold_right fold symbols (Option.map sym_prec sym |> Option.join)
  ;;

  let tr_action p symbol index =
    let id = function
      | { Ast.id = Some id; Ast.actual = _; _ } -> Some id.data
      | { Ast.id = None; _ } -> None
    in
    let sa_code = p.Ast.action
    and sa_args = List.map id p.Ast.prod in
    { sa_symbol = symbol; sa_index = index; sa_args; sa_code }
  ;;

  (** Create semantic action and item from given production `prod`, then register
      created action in `actions` and add item to `items`. *)
  let fold_prod symbol (actions, items, i) prod =
    let action, id = tr_action prod symbol i, IntMap.cardinal actions in
    let suffix = tr_symbols prod in
    let prec = get_precedence suffix prod.prec in
    let item = { i_suffix = suffix; i_action = id; i_prec = prec } in
    IntMap.add id action actions, item :: items, i + 1
  ;;

  let compare_prod_length a b = List.length b.Ast.prod - List.length a.Ast.prod

  (** Create item group from given rule `rule`, while registering all its actions in `actions`,
      then add group to `groups`. *)
  let fold_rule actions groups sym rule =
    let prods = List.sort compare_prod_length rule.Ast.prods in
    let actions, items, _ = List.fold_left (fold_prod sym) (actions, [], 0) prods in
    let group =
      { g_symbol = sym
      ; g_prefix = []
      ; g_items = List.rev items
      ; g_lookahead = TermSet.empty
      ; g_starting = false
      }
    in
    actions, NTermMap.add group.g_symbol group groups
  ;;

  (** `actions` is a map from action id to semantic action, for all semantic actions
        defined in the Ast.
      `groups` is a map from nonterminal id to a item group in a form { X → ε · β1, …, βn },
        where X → β1, …, X → βn are producions from grammar *)
  let actions, groups =
    let fold (actions, groups) rule =
      let sym, _ = Hashtbl.find nterm rule.Ast.id.data in
      if NTermMap.mem sym groups
      then actions, groups
      else fold_rule actions groups sym rule
    in
    List.fold_left fold (IntMap.empty, NTermMap.empty) A.ast.rules
  ;;

  let term = Hashtbl.to_seq_values term |> TermMap.of_seq
  let nterm = Hashtbl.to_seq_values nterm |> NTermMap.of_seq

  let term t =
    let loc = Lexing.dummy_pos, Lexing.dummy_pos in
    if t = Terminal.dummy
    then { ti_name = { data = "<UNKNOWN>"; loc }; ti_ty = None; ti_prec = None }
    else TermMap.find t term
  ;;

  let nterm n = NTermMap.find n nterm
  let group n = NTermMap.find n groups
end
