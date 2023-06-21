FILES=(
    api.lua
    attrib.lua
    big.lua
    bitwise.lua
    bwcoercion.lua
    calls.lua
    closure.lua
    code.lua
    constructs.lua
    coroutine.lua
    cstack.lua
    db.lua
    errors.lua
    events.lua
    files.lua
    gc.lua
    gengc.lua
    goto.lua
    heavy.lua
    literals.lua
    locals.lua
    main.lua
    math.lua
    nextvar.lua
    pm.lua
    sort.lua
    strings.lua
    tpack.lua
    utf8.lua
    vararg.lua
    verybig.lua
    all.lua
)

URL='https://github.com/lua/lua/raw/v5.4.0/testes'

rm -f input.lua
for file in "${FILES[@]}"; do
    echo $file
    echo "function ${file%.*}_lua ()" >> input.lua
    curl -sL "$URL/$file" | grep -av '^#' >> input.lua
    echo "end" >> input.lua
done
