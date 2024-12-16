# Continuation-passing style parser generator

## About

This program is a tool similar to [ocamlyacc](https://v2.ocaml.org/manual/lexyacc.html) and
[menhir](https://cambium.inria.fr/~fpottier/menhir/) that generates LALR(1) parsers. However,
unlike those, it uses the continuation-passing style, which allows for better type inference
and better optimization by the compiler.

## Features

- Focused on LALR(1) parsers, but can also generate LR(0), SLR and LR(1) parsers.
- Mostly compatible with ocamlyacc, supporting following features:
  - `%left`, `%right` and `%nonassoc` precedence and associativity declarations
  - `$1`, `$2`, ... style references to semantic values
  - `Parsing` module compability, including `Parsing.symbol_start` and similar functions
- Supports subset of menhir-specific features:
  - Named semantic values using `id=symbol` syntax
  - `$startpos`, `loc` and similar keywords
  - Parametric rules using `rule(param1, param2)` syntax
  - Subset of menhir's standard library, including `list`, `option`
  - `*`, `+` and `?` shorthand notation

Unsupported, but planned:

- Error recovery
- `%inline` rules
- Anonymous rules
- Parametric semantic actions

## Usage

To generate parser from `.mly` grammar definition, run:

```sh
cpspg [-o OUTPUT] INPUT
```

To use this tool as a part of dune build process, add following rule to `dune` file:

```dune
(rule
 (deps Parser.mly)
 (target Parser.cpspg.ml)
 (action
  (chdir %{workspace_root} (run cpspg -o %{target} %{deps}))))
```

## Building

This project is managed using `dune`. To build it, run:

```sh
dune build
```

Note that `lib/Parser.ml` is bootstrapped from `lib/Parser.mly`. When code generation changes, it
can be [promoted](https://dune.readthedocs.io/en/stable/concepts/promotion.html) to new version:

```sh
dune build @bootstrap
dune promote
```

## Why would I care about continuation-passing style?

LR parsers are essentialy finite automata with a stack containing various semantic values.
Such stack cannot be easily represented in a functional language, which is why `ocamlyacc`
and `menhir` resort to using tricks -- `ocamlyacc` uses `Obj.repr` as an escape hatch, while
`menhir` makes use of GADTs. Parsers generated this way are impossible to typecheck without explicit
type declarations. CPS allows to generate simpler and more type-safe code, which, as a bunus, is
also readable (sort of).
