%{
  open Ast
%}

%start chunk

%token EOF

%token KW_AND KW_BREAK KW_DO KW_ELSE KW_ELSEIF KW_END KW_FALSE KW_FOR
       KW_FUNCTION KW_GOTO KW_IF KW_IN KW_LOCAL KW_NIL KW_NOT KW_OR
       KW_REPEAT KW_RETURN KW_THEN KW_TRUE KW_UNTIL KW_WHILE

%token P_AND P_ASSIGN P_COLON P_COMMA P_CONCAT P_DBCOLON P_DOT P_DOTS
       P_EQ P_GE P_GT P_HAT P_IDIV P_LBRACE P_LBRACKET P_LE P_LENGTH
       P_LPAREN P_LT P_MINUS P_NE P_OR P_PERCENT P_PLUS P_RBRACE
       P_RBRACKET P_RPAREN P_SEMI P_SHL P_SHR P_SLASH P_STAR P_TILDE

%token<string> NAME STRING
%token<string> NUMERAL
%token<string> INT

%type<Ast.chunk> chunk
%type<Ast.block> block
%type<Ast.stat> stat
%type<(Ast.exp * Ast.block) list> stat_if_elseif
%type<Ast.block option> stat_if_else
%type<Ast.stat> stat_return
%type<string> label
%type<exp list> explist
%type<string list> namelist
%type<(string * string option) list> attnamelist
%type<string option> attrib
%type<Ast.exp> exp prefixexp
%type<Ast.var> var
%type<Ast.field list> tableconstructor fieldlist
%type<Ast.field> field
%type<Ast.exp list> args
%type<unit> fieldsep
%type<Ast.call> functioncall
%type<Ast.var list> varlist
%type<Ast.parlist * Ast.block> functiondef funcbody
%type<string list> funcpath
%type<Ast.funcname> funcname
%type<Ast.parlist> parlist

%nonassoc base
%nonassoc P_LPAREN
%left KW_OR
%left KW_AND
%left P_LT P_GT P_LE P_GE P_NE P_EQ
%left P_OR
%left P_TILDE
%left P_AND
%left P_SHR P_SHL
%right P_CONCAT
%left P_PLUS P_MINUS
%left P_STAR P_SLASH P_IDIV P_PERCENT
%nonassoc KW_NOT P_LENGTH P_MINUS_u P_TILDE_u
%right P_HAT


%%

chunk:
    | block EOF { $1 }
;

block:
    | /* empty */ { [] }
    | stat_return { [ $1 ] }
    | stat block  { $1 :: $2 }
;

stat:
    | P_SEMI                  { StatEmpty }
    | KW_DO block KW_END      { StatBlock $2 }
	  | label                   { StatLabel $1 }
    | KW_GOTO NAME            { StatGoto $2 }
    | functioncall %prec base { StatCall $1 }
    | KW_BREAK                { StatBreak }

    | varlist P_ASSIGN explist
      { StatAssign { vars = $1; value = $3 } }

    | KW_IF exp KW_THEN block stat_if_elseif stat_if_else KW_END
      { StatIf { arms = ($2, $4) :: $5; alt = $6 } }

    | KW_FOR NAME P_ASSIGN exp P_COMMA exp KW_DO block KW_END
      { StatFor { name = $2; init = $4; limit = $6; step = None; body = $8 } }

    | KW_FOR NAME P_ASSIGN exp P_COMMA exp P_COMMA exp KW_DO block KW_END
      { StatFor { name = $2; init = $4; limit = $6; step = Some $8; body = $10 } }

    | KW_FOR namelist KW_IN explist KW_DO block KW_END
      { StatGenericFor { vars = $2; value = $4; body = $6 } }

    | KW_WHILE exp KW_DO block KW_END
      { StatWhile { cond = $2; body = $4 } }

    | KW_REPEAT block KW_UNTIL exp
      { StatRepeat { body = $2; cond = $4 } }

    | KW_LOCAL KW_FUNCTION funcname funcbody
      { let params, body = $4 in StatFuncLocal { name = $3; params; body } }
    | KW_FUNCTION funcname funcbody
      { let params, body = $3 in StatFuncNamed { name = $2; params; body } }

    | KW_LOCAL attnamelist
      { StatLocal { vars = $2; value = None } }
    | KW_LOCAL attnamelist P_ASSIGN explist
      { StatLocal { vars = $2; value = Some $4 } }
;

stat_if_elseif:
    | /* empty */ { [] }
    | KW_ELSEIF exp KW_THEN block stat_if_elseif
      { ($2, $4) :: $5 }
;

stat_if_else:
    | /* empty */   { None }
    | KW_ELSE block { Some $2 }
;

stat_return:
    | KW_RETURN                { StatReturn [ ] }
    | KW_RETURN P_SEMI         { StatReturn [ ] }
    | KW_RETURN explist        { StatReturn $2 }
    | KW_RETURN explist P_SEMI { StatReturn $2 }
;

label:
    | P_DBCOLON NAME P_DBCOLON { $2 }
;

varlist:
    | var                 { [ $1 ] }
    | var P_COMMA varlist { $1 :: $3 }
;

explist:
    | exp                 { [ $1 ] }
    | exp P_COMMA explist { $1 :: $3 }
;

namelist:
    | NAME                  { [ $1 ] }
    | NAME P_COMMA namelist { $1 :: $3 }
;

attnamelist:
    | NAME attrib                     { [ $1, $2 ] }
    | NAME attrib P_COMMA attnamelist { ($1, $2) :: $4 }
;

attrib:
    | /* empty */    { None }
    | P_LT NAME P_GT { Some $2 }
;

exp:
    | KW_NIL           { ExpNil }
    | KW_TRUE          { ExpTrue }
    | KW_FALSE         { ExpFalse }
    | P_DOTS           { ExpDots }
    | INT              { ExpInt $1 }
    | NUMERAL          { ExpNumeral $1 }
    | STRING           { ExpString $1 }
    | tableconstructor { ExpTable $1 }

    | prefixexp   { $1 }
    | functiondef { let params, body = $1 in ExpFunction { params; body } }

    | exp P_PLUS    exp { ExpBinop { l = $1; r = $3; op = BinopAdd } }
    | exp P_MINUS   exp { ExpBinop { l = $1; r = $3; op = BinopSub } }
    | exp P_STAR    exp { ExpBinop { l = $1; r = $3; op = BinopMul } }
    | exp P_SLASH   exp { ExpBinop { l = $1; r = $3; op = BinopDiv } }
    | exp P_IDIV    exp { ExpBinop { l = $1; r = $3; op = BinopIDiv } }
    | exp P_PERCENT exp { ExpBinop { l = $1; r = $3; op = BinopMod } }
    | exp P_HAT     exp { ExpBinop { l = $1; r = $3; op = BinopExp } }
    | exp P_AND     exp { ExpBinop { l = $1; r = $3; op = BinopBitAnd } }
    | exp P_OR      exp { ExpBinop { l = $1; r = $3; op = BinopBitOr } }
    | exp P_TILDE   exp { ExpBinop { l = $1; r = $3; op = BinopBitXor } }
    | exp P_SHR     exp { ExpBinop { l = $1; r = $3; op = BinopBitShr } }
    | exp P_SHL     exp { ExpBinop { l = $1; r = $3; op = BinopBitShl } }
    | exp P_EQ      exp { ExpBinop { l = $1; r = $3; op = BinopEq } }
    | exp P_NE      exp { ExpBinop { l = $1; r = $3; op = BinopNe } }
    | exp P_LT      exp { ExpBinop { l = $1; r = $3; op = BinopLt } }
    | exp P_LE      exp { ExpBinop { l = $1; r = $3; op = BinopLe } }
    | exp P_GT      exp { ExpBinop { l = $1; r = $3; op = BinopGt } }
    | exp P_GE      exp { ExpBinop { l = $1; r = $3; op = BinopGe } }
    | exp KW_AND    exp { ExpBinop { l = $1; r = $3; op = BinopAnd } }
    | exp KW_OR     exp { ExpBinop { l = $1; r = $3; op = BinopOr } }
    | exp P_CONCAT  exp { ExpBinop { l = $1; r = $3; op = BinopConcat } }

    | P_MINUS  exp %prec P_MINUS_u { ExpUnop { x = $2; op = UnopNeg } }
    | P_TILDE  exp %prec P_TILDE_u { ExpUnop { x = $2; op = UnopBitNot } }
    | P_LENGTH exp                 { ExpUnop { x = $2; op = UnopLength } }
    | KW_NOT   exp                 { ExpUnop { x = $2; op = UnopNot } }
;

prefixexp:
    | var                   %prec base { ExpVar $1 }
    | functioncall          %prec base { ExpCall $1 }
    | P_LPAREN exp P_RPAREN %prec base { $2 }
;

var:
    | NAME                                { VarName $1 }
    | prefixexp P_LBRACKET exp P_RBRACKET { VarIndex { target = $1; index = $3 } }
    | prefixexp P_DOT NAME                { VarField { target = $1; field = $3 } }
;

tableconstructor:
    | P_LBRACE P_RBRACE           { [] }
    | P_LBRACE fieldlist P_RBRACE { $2 }
;

fieldlist:
    | field                    { [ $1 ] }
    | field fieldsep           { [ $1 ] }
    | field fieldsep fieldlist { $1 :: $3 }
;

field:
    | exp                                    { Field { value = $1 } }
    | P_LBRACKET exp P_RBRACKET P_ASSIGN exp { FieldIndex { index = $2; value = $5 } }
    | NAME P_ASSIGN exp                      { FieldName  { name = $1; value = $3 } }
;

fieldsep:
    | P_COMMA { () }
    | P_SEMI  { () }
;

functioncall:
    | var args                   { Call { callee = ExpVar $1; args = $2 } }
    | functioncall args          { Call { callee = ExpCall $1; args = $2 } }
    | P_LPAREN exp P_RPAREN args { Call { callee = $2; args = $4 } }

    | var P_COLON NAME args                   { CallMethod { callee = ExpVar $1; name = $3; args = $4 } }
    | functioncall P_COLON NAME args          { CallMethod { callee = ExpCall $1; name = $3; args = $4 } }
    | P_LPAREN exp P_RPAREN P_COLON NAME args { CallMethod { callee = $2; name = $5; args = $6 } }
;

args:
    | P_LPAREN P_RPAREN          { [] }
    | P_LPAREN explist P_RPAREN  { $2 }
    | STRING                     { [ ExpString $1 ] }
    | tableconstructor           { [ ExpTable $1] }
;

functiondef:
    | KW_FUNCTION funcbody { $2 }
;

funcbody:
    | P_LPAREN P_RPAREN block KW_END         { Parlist { params = []; variadic = false }, $3 }
    | P_LPAREN parlist P_RPAREN block KW_END { $2, $4 }
;

funcname:
    | funcpath              { Func { base = $1 } }
    | funcpath P_COLON NAME { FuncMethod { base = $1; name = $3 } }
;

funcpath:
    | NAME                { [ $1 ] }
    | NAME P_DOT funcpath { $1 :: $3 }
;

parlist:
    | NAME   { Parlist { params = [ $1 ]; variadic = false } }
    | P_DOTS { Parlist { params = []; variadic = true } }

    | NAME P_COMMA parlist
      { let Parlist { params; variadic } = $3 in
        Parlist { params = $1 :: params; variadic } }
;
