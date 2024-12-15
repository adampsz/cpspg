# Simple Lua 5.4 parser

Run:

```sh
bash input.sh # Download test input

cat input.lua | \
    GEN=cpspg dune exec --profile release ./main.exe

# Or GEN=mehir dune exec ...
# Or GEN=ocamlyacc dune exec ...
```
