type 'a node =
  { loc : Lexing.position * Lexing.position
  ; data : 'a
  }

type id = string node
type tid = string node
type ty = string node
type code = string node

type symbol =
  | NTerm of id
  | Term of tid

type decl =
  | DeclToken of ty option * tid list
  | DeclStart of ty option * id list
  | DeclType of ty * symbol list
  | DeclLeft of symbol list
  | DeclRight of symbol list
  | DeclNonassoc of symbol list

type producer =
  { id : id option
  ; actual : symbol
  }

type production =
  { prod : producer list
  ; action : code
  }

type rule =
  { id : id
  ; prods : production list
  }

type t =
  { header : code
  ; decls : decl list
  ; rules : rule list
  }
