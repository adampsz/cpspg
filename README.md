# Continuation-Passing Style Parser Generator

## Overview

This program is a tool similar to [ocamlyacc](https://v2.ocaml.org/manual/lexyacc.html) and
[menhir](https://cambium.inria.fr/~fpottier/menhir/) that generates LALR(1) parsers. However, unlike those, it uses the
continuation-passing style, which allows for better type inference and more efficient compiler optimizations.

## Features

- Specializes in generating LALR(1) parsers, with support for LR(0), SLR, and LR(1) parsers.
- Largely compatible with ocamlyacc, supporting:
  - `%left`, `%right`, and `%nonassoc` precedence and associativity declarations
  - `$1`, `$2`, etc., for referencing semantic values
  - Compatibility with the `Parsing` module, including functions like `Parsing.symbol_start`
- Supports a subset of menhir-specific features:
  - Named semantic values using the `id=symbol` syntax
  - Keywords such as `$startpos` and `$loc`
  - Parametric rules using the `rule(param1, param2)` syntax
  - A subset of menhir's standard library, including `list` and `option`
  - Shorthand notations `*`, `+`, and `?`

Planned but not yet supported:

- Error recovery
- `%inline` rules
- Anonymous rules
- Parametric semantic actions

## Usage

To generate a parser from a `.mly` grammar definition, use the following command:

```sh
cpspg [-o OUTPUT] INPUT
```

To integrate this tool into a dune build process, add the following rule to your `dune` file:

```dune
(rule
 (deps Parser.mly)
 (target Parser.ml)
 (action
  (chdir %{workspace_root} (run cpspg -o %{target} %{deps}))))
```

## Building

This project is managed using `dune`. To build it, execute:

```sh
dune build
```

Note that `lib/Parser.ml` is bootstrapped from `lib/Parser.mly`. When code generation changes, it can be
[promoted](https://dune.readthedocs.io/en/stable/concepts/promotion.html) to a new version:

```sh
dune build @bootstrap
dune promote
```

## Why Use Continuation-Passing Style?

LR parsers are essentially finite automata with a stack containing various semantic values. Representing such a stack in
a functional language is challenging. Traditional tools like `ocamlyacc` and `menhir` employ workarounds — `ocamlyacc`
uses `Obj.repr` as an escape hatch, while `menhir` utilizes GADTs. These approaches make it difficult to typecheck
parsers without explicit type declarations. CPS enables the generation of simpler, more type-safe code that is also more
readable (sort of).
