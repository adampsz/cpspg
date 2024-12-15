module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)
module TermMap = Map.Make (Automaton.Terminal)
module NTermMap = Map.Make (Automaton.Nonterminal)

module Run (S : Types.Settings) (A : Types.Ast) : Types.Grammar = struct
  open Automaton

  type value =
    | VDummy
    | VSymbol of symbol
    | VRule of string

  let sym_name = function
    | Ast.Term t -> t
    | Ast.NTerm n -> n
  ;;

  let header =
    let get_code = function
      | Ast.DeclCode code -> Some code
      | _ -> None
    in
    List.filter_map get_code A.ast.decls
  ;;

  (** [prec] is a mapping from precedence names to their [(left, right)] levels.
      Left-associative operators are represented with precedence levels [(p + 1, p)],
      right-associative operators with levels [(p, p + 1)], and non-associative operators
      with levels [(p, p)]. This encoding allows for straightforward comparison of precedence
      levels by comparing the left and right values, without needing to handle associativity separately. *)
  let prec =
    let prec = Hashtbl.create 64 in
    let iter_sym p sym =
      let sym = sym_name sym in
      if Hashtbl.mem prec sym.data
      then S.report_warn ~loc:sym.loc "duplicate precedence %s" sym.data;
      Hashtbl.replace prec sym.data p
    in
    let iter i = function
      | Ast.DeclLeft xs -> List.iter (iter_sym ((i * 2) + 1, i * 2)) xs
      | Ast.DeclRight xs -> List.iter (iter_sym (i * 2, (i * 2) + 1)) xs
      | Ast.DeclNonassoc xs -> List.iter (iter_sym (i * 2, i * 2)) xs
      | _ -> ()
    in
    List.iteri iter A.ast.decls;
    prec
  ;;

  (** [term] is a mapping from a terminal name to its id and info. *)
  let term =
    let term = Hashtbl.create 128 in
    let iter_token ty name =
      let prec = Hashtbl.find_opt prec name.data in
      let info = { ti_name = name; ti_ty = ty; ti_prec = prec } in
      if Hashtbl.mem term name.data
      then S.report_warn ~loc:name.loc "duplicate terminal symbol %s" name.data
      else Hashtbl.add term name.data (Hashtbl.length term |> Terminal.of_int, info)
    in
    let iter_decl = function
      | Ast.DeclToken (ty, ids) -> List.iter (iter_token ty) ids
      | _ -> ()
    in
    List.iter iter_decl A.ast.decls;
    term
  ;;

  let rules =
    let rules = Hashtbl.create 64 in
    let iter_rule rule =
      let name = rule.Ast.id in
      if Hashtbl.mem rules name.data
      then S.report_warn ~loc:name.loc "duplicate rule %s" name.data
      else Hashtbl.replace rules name.data rule
    in
    List.iter iter_rule A.ast.rules;
    rules
  ;;

  let nterm_id : (string * value list, Nonterminal.t) Hashtbl.t = Hashtbl.create 128
  let nterm_info : (Nonterminal.t, nterm_info * group) Hashtbl.t = Hashtbl.create 128
  let actions : (string * int, int * semantic_action) Hashtbl.t = Hashtbl.create 128

  let init_env loc params given =
    let name s = (sym_name s).data in
    let rec aux env = function
      | [], [] -> env
      | [], _ ->
        S.report_err ~loc "too many arguments";
        env
      | params, [] ->
        S.report_err ~loc "not enough arguments";
        List.fold_left (fun env p -> StringMap.add (name p) VDummy env) env params
      | p :: params, a :: given ->
        let env = StringMap.add (name p) a env in
        aux env (params, given)
    in
    aux StringMap.empty (params, given)
  ;;

  let tr_term name =
    match Hashtbl.find_opt term name.data with
    | Some (t, _) -> t
    | None ->
      S.report_err ~loc:name.loc "unknown terminal symbol %s" name.data;
      Terminal.dummy
  ;;

  let tr_action rule idx prod =
    let producer_id = function
      | { Ast.id = Some id; Ast.actual = _ } -> Some id.data
      | { Ast.id = None; Ast.actual = _ } -> None
    in
    match Hashtbl.find_opt actions (rule.Ast.id.data, idx) with
    | Some (id, _) -> id
    | None ->
      let args = List.map producer_id prod.Ast.prod in
      let action = { sa_args = args; sa_code = prod.Ast.action; sa_rule = rule.Ast.id }
      and id = Hashtbl.length actions in
      Hashtbl.add actions (rule.Ast.id.data, idx) (id, action);
      id
  ;;

  let rec tr_actual (env : value StringMap.t) actual : symbol =
    let sym = actual.Ast.symbol in
    let name = sym_name sym in
    match actual.symbol, StringMap.find_opt name.data env with
    | _, Some VDummy -> Term Terminal.dummy
    | _, Some (VSymbol id) -> id
    | _, Some (VRule rule) ->
      let rule = Hashtbl.find rules rule in
      NTerm (tr_args env actual.args |> instantiate rule)
    | Ast.Term t, None ->
      if actual.args <> []
      then S.report_err ~loc:name.loc "terminal symbols do not accept arguemtns";
      Term (tr_term t)
    | Ast.NTerm id, None ->
      (match Hashtbl.find_opt rules id.data with
       | None ->
         S.report_err ~loc:id.loc "unknown nonterminal symbol %s" id.data;
         Term Terminal.dummy
       | Some rule -> NTerm (tr_args env actual.args |> instantiate rule))

  and tr_production env rule idx prod =
    let get_prec p = Hashtbl.find_opt prec (sym_name p).data in
    { i_suffix = List.map (fun p -> tr_actual env p.Ast.actual) prod.Ast.prod
    ; i_action = tr_action rule idx prod
    ; i_prec = Option.bind prod.Ast.prec get_prec
    }

  and tr_args env args =
    let tr_arg = function
      | Ast.Arg actual ->
        let sym = actual.Ast.symbol in
        let name = sym_name sym in
        (match actual.symbol, StringMap.find_opt name.data env with
         | _, Some arg -> arg
         | Ast.NTerm id, None when actual.args = [] ->
           (match Hashtbl.find_opt rules id.data with
            | Some rule when rule.params <> [] -> VRule id.data
            | _ -> VSymbol (tr_actual env actual))
         | _, _ -> VSymbol (tr_actual env actual))
    in
    List.map tr_arg args

  and instantiate (rule : Ast.rule) (args : value list) : Nonterminal.t =
    let compare_item_len a b = -List.compare_lengths a.i_suffix b.i_suffix in
    match Hashtbl.find_opt nterm_id (rule.Ast.id.data, args) with
    | Some id -> id
    | None ->
      let id = Hashtbl.length nterm_id |> Nonterminal.of_int in
      Hashtbl.add nterm_id (rule.Ast.id.data, args) id;
      let env = init_env rule.Ast.id.loc rule.Ast.params args in
      let items = List.mapi (tr_production env rule) rule.prods in
      let info = { ni_name = rule.id; ni_starting = false }
      and group =
        { g_symbol = id
        ; g_prefix = []
        ; g_items = List.sort compare_item_len items
        ; g_lookahead = TermSet.empty
        ; g_starting = false
        }
      in
      Hashtbl.add nterm_info id (info, group);
      id
  ;;

  (* Find all starting points and fill [nterm_id] and [nterm_info] tables *)
  let _ =
    let iter_start name =
      match Hashtbl.find_opt rules name.data with
      | None -> S.report_err ~loc:name.loc "unknown starting symbol %s" name.data
      | Some rule when rule.params <> [] ->
        S.report_err ~loc:name.loc "starting rule %s cannot accept parameters" name.data
      | Some rule ->
        let id = instantiate rule [] in
        let info, group = Hashtbl.find nterm_info id in
        if info.ni_starting
        then S.report_warn ~loc:name.loc "rule %s is already marked as starting" name.data
        else Hashtbl.replace nterm_info id ({ info with ni_starting = true }, group)
    in
    let iter_decl = function
      | Ast.DeclStart (_, ids) -> List.iter iter_start ids
      | _ -> ()
    in
    List.iter iter_decl A.ast.decls
  ;;

  let symbols =
    let term = Hashtbl.to_seq_values term |> Seq.map (fun (t, _) -> Term t)
    and nterm = Hashtbl.to_seq_values nterm_id |> Seq.map (fun n -> NTerm n) in
    Seq.append term nterm |> List.of_seq |> List.sort compare
  ;;

  let term =
    let term = Hashtbl.to_seq_values term |> TermMap.of_seq in
    fun t -> TermMap.find t term
  ;;

  (* Collect all non-terminals and groups, also attach missing precedence levels to items. *)
  let nterm, group =
    let symbol_prec = function
      | Term t when t = Terminal.dummy -> None
      | Term t -> (term t).ti_prec
      | NTerm _ -> None
    in
    let attach_prec item =
      let prec =
        match item.i_prec with
        | Some prec -> Some prec
        | None -> List.find_map symbol_prec item.i_suffix
      in
      { item with i_prec = prec }
    in
    let attach_precs (id, (info, group)) =
      id, (info, { group with g_items = List.map attach_prec group.g_items })
    in
    let nterm = Hashtbl.to_seq nterm_info |> Seq.map attach_precs |> NTermMap.of_seq in
    (fun n -> NTermMap.find n nterm |> fst), fun n -> NTermMap.find n nterm |> snd
  ;;

  let actions = Hashtbl.to_seq_values actions |> IntMap.of_seq
end
