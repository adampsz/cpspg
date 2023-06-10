# Continuation-passing style parser generator

## Usage

To generate parser from `.mly` grammar definition, run:

```sh
dune exec -- cpspg [-o OUTPUT] INPUT
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

Note that `lib/Parser.ml` is bootstrapped from `lib/Parser.mly`. When code generation changes, it can be [promoted](https://dune.readthedocs.io/en/stable/concepts.html#promotion) to new version:

```sh
dune build @bootstrap
dune promote
```
