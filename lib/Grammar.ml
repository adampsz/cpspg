type decl =
  | DeclToken of string option * string list
  | DeclStart of string option * string list

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
