# Continuation-passing style parser generator

## Building

This project is managed using `dune`. To build it, run:

```sh
dune build
```

Note that `lib/Parser.ml` is bootstrapped from `lib/Parser.cpspg`. When code generation changes, it can be [promoted](https://dune.readthedocs.io/en/stable/concepts.html#promotion) to new version:

```sh
dune build @bootstrap
dune promote
```
