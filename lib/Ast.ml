type loc = Lexing.position * Lexing.position

type 'a node =
  { loc : loc
  ; data : 'a
  }

type keyword =
  | KwArg of int
  | KwStartpos
  | KwEndpos
  | KwSymbolstartpos
  | KwStartofs
  | KwEndofs
  | KwSymbolstartofs
  | KwLoc
  | KwSloc

type code = string * (keyword * loc) list

type symbol =
  | NTerm of string node
  | Term of string node

type decl =
  | DeclToken of string node option * string node list
  | DeclStart of string node option * string node list
  | DeclType of string node * symbol list
  | DeclLeft of symbol list
  | DeclRight of symbol list
  | DeclNonassoc of symbol list
  | DeclCode of string node

type actual =
  { symbol : symbol
  ; args : arg list
  }

and arg =
  | Arg of actual
  | ArgInline of
      { prod : producer list
      ; action : code node
      }

and producer =
  { id : string node option
  ; actual : actual
  }

type production =
  { prod : producer list
  ; prec : symbol option
  ; action : code node
  }

type rule =
  { id : string node
  ; inline : bool
  ; params : symbol list
  ; prods : production list
  }

type t =
  { decls : decl list
  ; rules : rule list
  }

let dummy = { decls = []; rules = [] }
