type kind =
  | LR0
  | SLR
  | LR1
  | LALR

module IntMap = Map.Make (Int)

module type Settings = sig
  val kind : kind

  (* Codegen *)
  val positions : bool
  val line_directives : bool
  val comments : bool
  val readable_ids : bool

  (* Callbacks *)
  val on_conflict : int -> Automaton.Terminal.t -> Automaton.action list -> unit
end

module type Ast = sig
  val ast : Grammar.t
end

module type Grammar = sig
  val header : string
  val term : Automaton.Terminal.t -> Automaton.term_info
  val nterm : Automaton.Nonterminal.t -> Automaton.nterm_info
  val group : Automaton.Nonterminal.t -> Automaton.group
  val symbols : Automaton.symbol list
  val actions : Automaton.semantic_action IntMap.t
end

module type Automaton = sig
  val automaton : Automaton.t
end

module type Code = sig
  val write : Format.formatter -> unit
end
