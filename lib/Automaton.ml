module Terminal : sig
  type t

  val dummy : t
  val compare : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
end = struct
  type t = int

  let dummy = -1
  let compare = ( - )
  let of_int x = x
  let to_int x = x
end

module Nonterminal : sig
  type t

  val compare : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
end = struct
  type t = int

  let compare = ( - )
  let of_int x = x
  let to_int x = x
end

module Symbol = struct
  type t =
    | Term of Terminal.t
    | NTerm of Nonterminal.t

  let compare : t -> t -> int = compare
end

module SymbolMap = Map.Make (Symbol)
module TermSet = Set.Make (Terminal)
module IntMap = Map.Make (Int)

type loc = Ast.loc

type 'a node = 'a Ast.node =
  { loc : loc
  ; data : 'a
  }

type symbol = Symbol.t =
  | Term of Terminal.t
  | NTerm of Nonterminal.t

type prec = int * int

(** Suffix of LR(0)/LR(1) *)
type item =
  { i_suffix : symbol list
  ; i_action : int (** Production/semantic action id *)
  ; i_prec : prec option
  }

(** Group of LR(0)/LR(1) items with common nonterminal and prefix.
    INVARIANT: Items are sorted bu suffix lenght in increasing order. *)
type group =
  { g_symbol : Nonterminal.t (** Nonterminal. *)
  ; g_prefix : symbol list (** Prefix common to all items in this group. *)
  ; g_items : item list (** Items. *)
  ; g_lookahead : TermSet.t (** Lookahead symbols. Empty for LR(0) group. *)
  ; g_starting : bool (** Whether symbol is a starting symbol from augmented grammar. *)
  }

type semantic_action =
  { sa_symbol : Nonterminal.t
  ; sa_index : int
  ; sa_args : string option list
  ; sa_code : Ast.code node
  }

type action =
  | Shift (** Eat one temrinal from input. *)
  | Reduce of (int * int) (** `Reduce (i, j)` - reduce j-th item from i-th group. *)

(** LR(0)/LR(1) state.
    INVARIANT: groups are sorted by prefix length, in descending order. *)
type state =
  { s_kernel : group list
  (** Item groups, sorted descending by prefix length, and then alphabetically. *)
  ; s_closure : group list
  (** Additional item groups added by CLOSURE, not present in kernel *)
  ; s_goto : int SymbolMap.t (** Successors *)
  ; s_action : (TermSet.t * action) list
  (** Map from lookahead terminal symbol to
      the corresponding parsing decision. *)
  }

type term_info =
  { ti_name : string node
  ; ti_ty : string node option
  ; ti_prec : prec option
  }

type nterm_info =
  { ni_name : string node
  ; ni_starting : bool
  }

type t =
  { a_header : string node list
  ; a_actions : semantic_action IntMap.t
  ; a_states : state IntMap.t
  ; a_starting : (Nonterminal.t * int) list
  }

let equal_groups a b =
  { a with g_lookahead = TermSet.empty } = { b with g_lookahead = TermSet.empty }
  && TermSet.equal a.g_lookahead b.g_lookahead
;;

let equal_states a b =
  { a with s_kernel = [] } = { b with s_kernel = [] }
  && List.equal equal_groups a.s_kernel b.s_kernel
;;

let merge_groups a b =
  let empty = TermSet.empty in
  assert ({ a with g_lookahead = empty } = { b with g_lookahead = empty });
  { a with g_lookahead = TermSet.union a.g_lookahead b.g_lookahead }
;;

let merge_states a b =
  let merge_shift _ a b =
    assert (a = b);
    Some a
  in
  { s_kernel = List.map2 merge_groups a.s_kernel b.s_kernel
  ; s_closure = List.map2 merge_groups a.s_closure b.s_closure
  ; s_goto = SymbolMap.union merge_shift a.s_goto b.s_goto
  ; s_action = []
  }
;;

let shifts_item symbol = function
  | { i_suffix = sym :: _; _ } when sym = symbol -> true
  | _ -> false
;;

let shifts_group symbol group = List.exists (shifts_item symbol) group.g_items

let shift_item symbol item =
  match item.i_suffix with
  | sym :: suffix when sym = symbol -> Some { item with i_suffix = suffix }
  | _ -> None
;;

let shift_group symbol group =
  match List.filter_map (shift_item symbol) group.g_items with
  | [] -> None
  | items -> Some { group with g_items = items; g_prefix = symbol :: group.g_prefix }
;;

let shift_state symbol state =
  match List.filter_map (shift_group symbol) (state.s_kernel @ state.s_closure) with
  | [] -> None
  | kernel ->
    Some { s_kernel = kernel; s_closure = []; s_goto = SymbolMap.empty; s_action = [] }
;;

let item_of_starting_symbol symbol =
  { i_suffix = [ NTerm symbol ]; i_action = -1; i_prec = None }
;;

let group_of_starting_symbol symbol =
  let g_items = [ item_of_starting_symbol symbol ]
  and g_lookahead = TermSet.empty in
  { g_symbol = symbol; g_prefix = []; g_items; g_lookahead; g_starting = true }
;;

let state_of_starting_symbol symbol =
  let kernel = [ group_of_starting_symbol symbol ] in
  { s_kernel = kernel; s_closure = []; s_goto = SymbolMap.empty; s_action = [] }
;;
