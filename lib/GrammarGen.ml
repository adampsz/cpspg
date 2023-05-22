module IntMap = Map.Make (Int)
module TermMap = Map.Make (Automaton.Terminal)
module NTermMap = Map.Make (Automaton.Nonterminal)

module Run (S : Types.Settings) (A : Types.Ast) : Types.Grammar = struct
  open Automaton

  let header = A.ast.header.data
  let term = Hashtbl.create 16
  let nterm = Hashtbl.create 16

  (* Define terminals *)
  let _ =
    let iter_token ty name =
      let name = name.Grammar.data in
      let info = { ti_name = name; ti_ty = ty } in
      Hashtbl.replace term name (Hashtbl.length term |> Terminal.of_int, info)
    in
    let iter_decl = function
      | Grammar.DeclToken (ty, ids) ->
        let ty = Option.map (fun x -> x.Grammar.data) ty in
        List.iter (iter_token ty) ids
      | _ -> ()
    in
    List.iter iter_decl A.ast.decls
  ;;

  (* Define non-terminals *)
  let _ =
    let iter_rule rule =
      let name = rule.Grammar.id.data in
      let info = { ni_name = name; ni_starting = false } in
      Hashtbl.replace nterm name (Hashtbl.length nterm |> Nonterminal.of_int, info)
    in
    List.iter iter_rule A.ast.rules
  ;;

  (* Mark starting symbols *)
  let _ =
    let iter_start name =
      let name = name.Grammar.data in
      let id, info = Hashtbl.find nterm name in
      Hashtbl.replace nterm name (id, { info with ni_starting = true })
    in
    let iter_decl = function
      | Grammar.DeclStart (_, ids) -> List.iter iter_start ids
      | _ -> ()
    in
    List.iter iter_decl A.ast.decls
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

  and tr_action p symbol index =
    let id = function
      | { Grammar.id = None; Grammar.actual = Grammar.NTerm name; _ } -> Some name.data
      | { Grammar.id = Some id; _ } -> Some id.data
      | { Grammar.id = None; _ } -> None
    in
    let sa_code = p.Grammar.action.data in
    let sa_args = List.map id p.Grammar.prod in
    { sa_symbol = symbol; sa_index = index; sa_args; sa_code }
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
    let g_symbol = fst (Hashtbl.find nterm rule.Grammar.id.data)
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

  include A
end
