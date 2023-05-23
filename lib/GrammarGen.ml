module IntMap = Map.Make (Int)
module TermMap = Map.Make (Automaton.Terminal)
module NTermMap = Map.Make (Automaton.Nonterminal)

module Run (S : Types.Settings) (A : Types.Ast) : Types.Grammar = struct
  open Automaton

  type prec_level =
    | PrecTerm of Terminal.t
    | PrecDummy of string

  let header = A.ast.header
  let term = Hashtbl.create 16
  let nterm = Hashtbl.create 16
  let prec = Hashtbl.create 16

  (* Define terminals *)
  let _ =
    let iter_token ty name =
      let info = { ti_name = name; ti_ty = ty; ti_prec = None } in
      Hashtbl.replace term name.data (Hashtbl.length term |> Terminal.of_int, info)
    in
    let iter_decl = function
      | Grammar.DeclToken (ty, ids) -> List.iter (iter_token ty) ids
      | _ -> ()
    in
    List.iter iter_decl A.ast.decls
  ;;

  (* Define non-terminals *)
  let _ =
    let iter_rule rule =
      let name = rule.Grammar.id in
      let info = { ni_name = name; ni_starting = false } in
      Hashtbl.replace nterm name.data (Hashtbl.length nterm |> Nonterminal.of_int, info)
    in
    List.iter iter_rule A.ast.rules
  ;;

  (* Mark starting symbols *)
  let _ =
    let iter_start name =
      let id, info = Hashtbl.find nterm name.data in
      Hashtbl.replace nterm name.data (id, { info with ni_starting = true })
    in
    let iter_decl = function
      | Grammar.DeclStart (_, ids) -> List.iter iter_start ids
      | _ -> ()
    in
    List.iter iter_decl A.ast.decls
  ;;

  (* Define precedence levels *)
  let _ =
    let iter_sym p sym =
      let info, name =
        match sym with
        | Grammar.Term t -> Hashtbl.find_opt term t.data, t.data
        | Grammar.NTerm n -> None, n.data
      in
      match info with
      | None -> Hashtbl.replace prec (PrecDummy name) p
      | Some (id, info) ->
        Hashtbl.replace prec (PrecTerm id) p;
        Hashtbl.replace term name (id, { info with ti_prec = Some p })
    in
    let iter i = function
      | Grammar.DeclLeft xs -> List.iter (iter_sym ((i * 2) + 1, i * 2)) xs
      | Grammar.DeclRight xs -> List.iter (iter_sym (i * 2, (i * 2) + 1)) xs
      | Grammar.DeclNonassoc xs -> List.iter (iter_sym (i * 2, i * 2)) xs
      | _ -> ()
    in
    List.iteri iter A.ast.decls
  ;;

  let symbols =
    let term = Hashtbl.to_seq_values term |> Seq.map (fun (t, _) -> Term t)
    and nterm = Hashtbl.to_seq_values nterm |> Seq.map (fun (n, _) -> NTerm n) in
    List.of_seq term @ List.of_seq nterm |> List.sort compare
  ;;

  let tr_symbol = function
    | Grammar.Term n -> Term (Hashtbl.find term n.data |> fst)
    | Grammar.NTerm n -> NTerm (Hashtbl.find nterm n.data |> fst)
  ;;

  let tr_symbols p = List.map (fun p -> tr_symbol p.Grammar.actual) p.Grammar.prod

  let get_precedence symbols p =
    let sym_prec p =
      match Hashtbl.find_opt term p.data with
      | Some (id, _) -> Hashtbl.find prec (PrecTerm id)
      | None -> Hashtbl.find prec (PrecDummy p.data)
    in
    let fold sym acc =
      match sym, acc with
      | _, Some prec -> Some prec
      | Term t, None -> Hashtbl.find_opt prec (PrecTerm t)
      | NTerm _, None -> None
    in
    List.fold_right fold symbols (Option.map sym_prec p)
  ;;

  let tr_action p symbol index =
    let id = function
      | { Grammar.id = None; Grammar.actual = Grammar.NTerm name; _ } -> Some name.data
      | { Grammar.id = Some id; _ } -> Some id.data
      | { Grammar.id = None; _ } -> None
    in
    let sa_code = p.Grammar.action
    and sa_args = List.map id p.Grammar.prod in
    { sa_symbol = symbol; sa_index = index; sa_args; sa_code }
  ;;

  (** Create semantic action and item from given production `prod`, then register
      created action in `actions` and add item to `items`. *)
  let fold_prod symbol (actions, items, i) prod =
    let action, id = tr_action prod symbol i, IntMap.cardinal actions + 1 in
    let suffix = tr_symbols prod in
    let prec = get_precedence suffix prod.prec in
    let item = { i_suffix = suffix; i_action = id; i_prec = prec } in
    IntMap.add id action actions, item :: items, i + 1
  ;;

  let compare_prod_length a b = List.length b.Grammar.prod - List.length a.Grammar.prod

  (** Create item group from given rule `rule`, while registering all its actions in `actions`,
      then add group to `groups`. *)
  let fold_rule (actions, groups) rule =
    let prods = List.sort compare_prod_length rule.Grammar.prods in
    let g_symbol = fst (Hashtbl.find nterm rule.Grammar.id.data)
    and g_lookahead = TermSet.empty in
    let actions, items, _ = List.fold_left (fold_prod g_symbol) (actions, [], 0) prods in
    let g_items = List.rev items in
    let group = { g_symbol; g_prefix = []; g_items; g_lookahead; g_starting = false } in
    actions, NTermMap.add group.g_symbol group groups
  ;;

  (** `actions` is a map from action id to semantic action, for all semantic actions
        defined in the grammar.
      `groups` is a map from nonterminal id to a item group in a form { X → ε · β1, …, βn },
        where X → β1, …, X → βn are producions from grammar *)
  let actions, groups =
    List.fold_left fold_rule (IntMap.empty, NTermMap.empty) A.ast.rules
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