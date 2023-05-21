type 'a node =
  { range : Lexing.position * Lexing.position
  ; data : 'a
  }

type decl =
  | DeclToken of string option * string list
  | DeclStart of string list
  | DeclType of string * string list
  | DeclLeft of string list
  | DeclRight of string list
  | DeclNonassoc of string list

type actual =
  | NTerm of string
  | Term of string

type producer =
  { id : string option
  ; actual : actual
  }

type production =
  { prod : producer list
  ; action : string
  }

type rule =
  { id : string
  ; prods : production list
  }

type t =
  { header : string
  ; decls : decl list
  ; rules : rule list
  }
