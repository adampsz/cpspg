type unop =
  | UnopNeg
  | UnopBitNot
  | UnopLength
  | UnopNot

type binop =
  | BinopAdd
  | BinopSub
  | BinopMul
  | BinopDiv
  | BinopIDiv
  | BinopMod
  | BinopExp
  | BinopBitAnd
  | BinopBitOr
  | BinopBitXor
  | BinopBitShr
  | BinopBitShl
  | BinopEq
  | BinopNe
  | BinopLt
  | BinopLe
  | BinopGt
  | BinopGe
  | BinopAnd
  | BinopOr
  | BinopConcat

type funcname =
  | Func of { base : string list }
  | FuncMethod of
      { base : string list
      ; name : string
      }

type parlist =
  | Parlist of
      { params : string list
      ; variadic : bool
      }

type call =
  | Call of
      { callee : exp
      ; args : exp list
      }
  | CallMethod of
      { callee : exp
      ; name : string
      ; args : exp list
      }

and field =
  | Field of { value : exp }
  | FieldIndex of
      { index : exp
      ; value : exp
      }
  | FieldName of
      { name : string
      ; value : exp
      }

and var =
  | VarName of string
  | VarIndex of
      { target : exp
      ; index : exp
      }
  | VarField of
      { target : exp
      ; field : string
      }

and exp =
  | ExpNil
  | ExpTrue
  | ExpFalse
  | ExpDots
  | ExpInt of string
  | ExpNumeral of string
  | ExpString of string
  | ExpTable of field list
  | ExpVar of var
  | ExpCall of call
  | ExpBinop of
      { l : exp
      ; r : exp
      ; op : binop
      }
  | ExpUnop of
      { x : exp
      ; op : unop
      }
  | ExpFunction of
      { params : parlist
      ; body : block
      }

and stat =
  | StatEmpty
  | StatBlock of block
  | StatLabel of string
  | StatGoto of string
  | StatCall of call
  | StatReturn of exp list
  | StatBreak
  | StatAssign of
      { vars : var list
      ; value : exp list
      }
  | StatFor of
      { name : string
      ; init : exp
      ; limit : exp
      ; step : exp option
      ; body : block
      }
  | StatGenericFor of
      { vars : string list
      ; value : exp list
      ; body : block
      }
  | StatIf of
      { arms : (exp * block) list
      ; alt : block option
      }
  | StatWhile of
      { cond : exp
      ; body : block
      }
  | StatRepeat of
      { body : block
      ; cond : exp
      }
  | StatFuncLocal of
      { name : funcname
      ; params : parlist
      ; body : block
      }
  | StatFuncNamed of
      { name : funcname
      ; params : parlist
      ; body : block
      }
  | StatLocal of
      { vars : (string * string option) list
      ; value : exp list option
      }

and block = stat list

type chunk = block
