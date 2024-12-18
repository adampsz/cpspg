module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)
module TermMap = Map.Make (Automaton.Terminal)
module NTermMap = Map.Make (Automaton.Nonterminal)

type value =
  | VDummy (* error value *)
  | VSymbol of Automaton.symbol (* ordinary symbol *)
  | VInline of Automaton.item list (* %inline symbol *)
  | VRule of Ast.rule (* higher order rule. *)

(** Standard library, copied from [Standard.mly] file into [Standard.ml] by dune *)
module Std = struct
  let lexbuf = Lexing.from_string Standard.contents
  let _ = Lexing.set_filename lexbuf "<standard.mly>"
  let ast = Parser.grammar Lexer.main lexbuf
end

(** [equal] and [hash] functions that compare rules by their ids only *)
module Rule = struct
  type t = Ast.rule

  let equal r1 r2 = r1.Ast.id.data == r2.Ast.id.data
  let hash r = Hashtbl.hash r.Ast.id.data
end

(** [equal] and [hash] function that compare [value]s using [Rule] module above *)
module Value = struct
  type t = value

  let equal v1 v2 =
    match v1, v2 with
    | VDummy, VDummy -> true
    | VSymbol s1, VSymbol s2 -> s1 = s2
    | VInline i1, VInline i2 -> i1 = i2
    | VRule r1, VRule r2 -> Rule.equal r1 r2
    | _, _ -> false
  ;;

  let hash = function
    | VDummy -> Hashtbl.hash 0
    | VSymbol s -> Hashtbl.hash (1, s)
    | VInline i -> Hashtbl.hash (2, i)
    | VRule r -> Hashtbl.hash (3, Rule.hash r)
  ;;
end

(** Specialized hashmap that is used to store rule instances, indexed by rule and its arguments.
    This map uses [Rule] and [Value] modules from above, which simplifies grammar generation while
    comparing rules only by their ids *)
module InstanceMap = Hashtbl.Make (struct
    type t = Ast.rule * value list

    let equal (r1, v1) (r2, v2) = Rule.equal r1 r2 && List.equal Value.equal v1 v2
    let hash (r, vs) = Hashtbl.hash (Rule.hash r, List.map Value.hash vs)
  end)

module Run (S : Types.Settings) (A : Types.Ast) : Types.Grammar = struct
  open Automaton

  let nterm_id = InstanceMap.create 128
  let nterm_info = Hashtbl.create 128
  let actions = Hashtbl.create 128

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

  (** All known rules, both from given grammar and the standard library *)
  let rules =
    let rules = Hashtbl.create 64 in
    let iter_rule rule =
      let name = rule.Ast.id in
      if Hashtbl.mem rules name.data
      then S.report_warn ~loc:name.loc "duplicate rule %s" name.data
      else Hashtbl.add rules name.data rule
    and iter_std rule =
      let name = rule.Ast.id in
      if not (Hashtbl.mem rules name.data) then Hashtbl.add rules name.data rule
    in
    List.iter iter_rule A.ast.rules;
    List.iter iter_std Std.ast.rules;
    rules
  ;;

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

  let tr_action (prod : Ast.producer list) (code : Ast.code node) =
    let get_args = function
      | { Ast.id = None; Ast.actual = _; _ } -> None
      | { Ast.id = Some id; _ } -> Some id.data
    in
    let args = List.map get_args prod in
    match Hashtbl.find_opt actions (code, args) with
    | Some (id, _) -> id
    | None ->
      let action = { sa_args = args; sa_code = code }
      and id = Hashtbl.length actions in
      Hashtbl.add actions (code, args) (id, action);
      id
  ;;

  let tr_values values =
    let append_one x xxs = List.map (fun xs -> x :: xs) xxs
    and append_some yys xxs =
      List.map (fun ys -> List.map (List.rev_append ys) xxs) yys |> List.flatten
    in
    let f (sym, arg) = function
      | VDummy ->
        let s = Term Terminal.dummy in
        append_one s sym, append_one None arg
      | VRule _ ->
        let s = Term Terminal.dummy in
        append_one s sym, append_one None arg
      | VSymbol s -> append_one s sym, append_one None arg
      | VInline items ->
        let s = List.map (fun i -> i.i_suffix) items
        and a = List.map (fun i -> [ i.i_action ]) items in
        append_some s sym, append_some a arg
    in
    let sym, arg = List.fold_left f ([ [] ], [ [] ]) values in
    List.map List.rev sym, List.map List.rev arg
  ;;

  let rec tr_actual env actual : value =
    let sym = actual.Ast.symbol in
    let name = sym_name sym in
    let get_nterm id args =
      match Hashtbl.find_opt rules id.data with
      | None ->
        S.report_err ~loc:id.loc "unknown nonterminal symbol %s" id.data;
        VDummy
      | Some rule when args = [] && rule.params <> [] -> VRule rule
      | Some rule -> tr_args env actual.args |> instantiate rule
    in
    match actual.symbol, StringMap.find_opt name.data env with
    | _, Some (VRule rule) when actual.args <> [] ->
      instantiate rule (tr_args env actual.args)
    | _, Some value ->
      if actual.args <> []
      then S.report_err ~loc:name.loc "this value does not accept arguments";
      value
    | Ast.Term t, None ->
      if actual.args <> []
      then S.report_err ~loc:name.loc "terminal symbols do not accept arguments";
      VSymbol (Term (tr_term t))
    | Ast.NTerm id, None -> get_nterm id actual.args

  and tr_production env prod =
    let values = List.map (fun p -> tr_actual env p.Ast.actual) prod.Ast.prod in
    let get_prec p = Hashtbl.find_opt prec (sym_name p).data in
    let get_group suffix args =
      { i_suffix = suffix
      ; i_action = Some { ac_id = tr_action prod.prod prod.action; ac_args = args }
      ; i_prec = Option.bind prod.Ast.prec get_prec
      }
    in
    let sym, arg = tr_values values in
    List.map2 get_group sym arg

  and tr_args env args =
    let tr_arg = function
      | Ast.Arg actual -> tr_actual env actual
      | Ast.ArgInline { prod; action } ->
        VInline (tr_production env { prod; action; prec = None })
    in
    List.map tr_arg args

  and instantiate rule args =
    let compare_item_len a b = -List.compare_lengths a.i_suffix b.i_suffix in
    match InstanceMap.find_opt nterm_id (rule, args) with
    | Some id ->
      if rule.inline
      then VInline (Hashtbl.find nterm_info id |> snd).g_items
      else VSymbol (NTerm id)
    | None ->
      let id = InstanceMap.length nterm_id |> Nonterminal.of_int in
      InstanceMap.add nterm_id (rule, args) id;
      let env = init_env rule.Ast.id.loc rule.Ast.params args in
      let items = List.map (tr_production env) rule.prods |> List.flatten in
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
      if rule.inline then VInline group.g_items else VSymbol (NTerm id)
  ;;

  (* Find all starting points and fill [nterm_id] and [nterm_info] tables *)
  let _ =
    let start name rule =
      match instantiate rule [] with
      | VDummy | VSymbol (Term _) ->
        S.report_err ~loc:name.loc "starting rule %s is invalid" name.data
      | VRule _ ->
        S.report_err ~loc:name.loc "starting rule %s cannot accept parameters" name.data
      | VInline _ ->
        S.report_err ~loc:name.loc "starting rule %s cannot be inline" name.data
      | VSymbol (NTerm id) ->
        let info, group = Hashtbl.find nterm_info id in
        if info.ni_starting
        then S.report_warn ~loc:name.loc "duplicate start declaration of %s" name.data
        else Hashtbl.replace nterm_info id ({ info with ni_starting = true }, group)
    in
    let iter_start name =
      match Hashtbl.find_opt rules name.data with
      | None -> S.report_err ~loc:name.loc "unknown starting symbol %s" name.data
      | Some rule -> start name rule
    in
    let iter_decl = function
      | Ast.DeclStart (_, ids) -> List.iter iter_start ids
      | _ -> ()
    in
    List.iter iter_decl A.ast.decls
  ;;

  let symbols =
    let term = Hashtbl.to_seq_values term |> Seq.map (fun (t, _) -> Term t)
    and nterm = InstanceMap.to_seq_values nterm_id |> Seq.map (fun n -> NTerm n) in
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
