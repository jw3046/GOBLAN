/* Ocamlyacc parser for GOBLAN */

%{
open Ast;;
let get1 (a,_,_,_) = a;;
let get2 (_,a,_,_) = a;;
let get3 (_,_,a,_) = a;;
let get4 (_,_,_,a) = a;;
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMI COMMA PERIOD
%token PLUS MINUS TIMES DIVIDE MODULO FPLUS FMINUS FTIMES FDIVIDE
%token ASSIGN EQ NEQ REQ RNEQ LT LEQ GT GEQ AND OR NOT ARROW
%token IF ELSE FOR IN WHILE BREAK CONTINUE RETURN
%token BOOL INT FLOAT CHAR STRING LIST GRAPH NEW VOID
%token TRUE FALSE DATA DO CATCH SELF PARENT CHILD NEIGHBORS MESSAGE
%token PASS ADD TO REMOVE FROM RUN NULL INFINITY
%token <string> ID
%token <float>  FLT_LIT
%token <int>    INT_LIT
%token <string> STR_LIT CHR_LIT
%token <string> NODE_TYP TUPLE_TYP
%token EOF

%right ASSIGN
%right TO FROM
%left OR
%left AND
%left EQ NEQ REQ RNEQ
%left LT GT LEQ GEQ
%left PLUS FPLUS MINUS FMINUS
%left TIMES FTIMES DIVIDE FDIVIDE MODULO
%left NOT NEG
%left PERIOD

%start program
%type <Ast.program> program

%%

program:
  decls EOF                            { $1 }

decls:
   /* nothing */                       { [], [], [], [] }
 | decls vdecl                         { ($2 :: get1 $1), get2 $1, get3 $1, get4 $1 }
 | decls tdecl                         { get1 $1, ($2 :: get2 $1), get3 $1, get4 $1 }
 | decls ndecl                         { get1 $1, get2 $1, ($2 :: get3 $1), get4 $1 }
 | decls fdecl                         { get1 $1, get2 $1, get3 $1, ($2 :: get4 $1) }

vdecl:
    typ ID SEMI { ($1, $2) }

vdecl_list:
    /* nothing */                      { [] }
  | vdecl_list vdecl                   { $2 :: $1 }

typ:
    INT                                { Int }
  | BOOL                               { Bool }
  | VOID                               { Void }
  | CHAR                               { Char }
  | STRING                             { Str }
  | FLOAT                              { Float }
  | NODE_TYP                           { NodeTyp($1) }
  | GRAPH NODE_TYP                     { GraphTyp($2) }
  | LIST typ                           { ListTyp($2) }
  | TUPLE_TYP                          { TupleTyp($1) }

ndecl:
   NODE_TYP LBRACE n_data n_do n_catch RBRACE
                                       { { n_typ = $1;
                                           n_data = $3;
                                           n_do = $4;
                                           n_catch = $5 } }

n_data:
   DATA LBRACE vdecl_list RBRACE       { { attributes = List.rev $3} }

n_do:
   typ DO LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
                                       { { typ = $1;
                                           formals = $4;
                                           locals = List.rev $7;
                                           body = List.rev $8 } }

n_catch:
   CATCH LBRACE vdecl_list stmt_list RBRACE
                                       { { locals = $3;
                                           body = $4 } }

tdecl:
    TUPLE_TYP LBRACE vdecl_list RBRACE { { typ = $1;
                                           attributes = List.rev $3 } }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
                                       { { typ = $1;
                                           fname = $2;
                                           formals = $4;
                                           locals = List.rev $7;
                                           body = List.rev $8 } }

formals_opt:
    /* nothing */                      { [] }
  | formal_list                        { List.rev $1 }

formal_list:
    typ ID                             { [($1,$2)] }
  | formal_list COMMA typ ID           { ($3,$4) :: $1 }

stmt_list:
    /* nothing */                      { [] }
  | stmt_list stmt                     { $2 :: $1 }

stmt:
    expr SEMI                          { Expr $1 }
  | LBRACE stmt_list RBRACE            { Block(List.rev $2) }
  | RETURN SEMI                        { ReturnNoExpr }
  | RETURN expr SEMI                   { Return $2 }
  | IF LPAREN expr RPAREN LBRACE stmt RBRACE
                                       { If($3, $6, Block([])) }
  | IF LPAREN expr RPAREN LBRACE stmt RBRACE ELSE LBRACE stmt RBRACE
                                       { If($3, $6, $10) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN LBRACE stmt_list RBRACE
                                       { For($3, $5, $7, $10) }
  | FOR LPAREN expr IN expr RPAREN LBRACE stmt_list RBRACE
                                       { ForEach($3, $5, $8)}
  | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE
                                       { While($3, $6) }
  | BREAK SEMI                         { Break }
  | CONTINUE SEMI                      { Continue }
  | PASS expr ARROW expr SEMI          { Pass($2, $4) }
  
expr_opt:
    /* nothing */                      { Noexpr }
  | expr                               { $1 }

expr:
    expr PERIOD  ID                    { Member($1, $3) }
  | TRUE                               { BoolLit(true) }
  | FALSE                              { BoolLit(false) }
  | SELF                               { Self }
  | MESSAGE                            { Message }
  | ID                                 { Id($1) }
  | PARENT                             { Parent }
  | CHILD                              { Child }
  | NEIGHBORS                          { Neighbors }
  | FLT_LIT                            { FloatLit($1) }
  | INT_LIT                            { IntLit($1) }
  | STR_LIT                            { StrLit($1) }
  | CHR_LIT                            { ChrLit($1) }
  | expr PLUS    expr                  { Binop($1, Add, $3) }
  | expr MINUS   expr                  { Binop($1, Sub, $3) }
  | expr TIMES   expr                  { Binop($1, Mult, $3) }
  | expr DIVIDE  expr                  { Binop($1, Div, $3) }
  | expr MODULO  expr                  { Binop($1, Mod, $3) }
  | expr FPLUS   expr                  { Binop($1, FAdd, $3) }
  | expr FMINUS  expr                  { Binop($1, FMinus, $3) }
  | expr FTIMES  expr                  { Binop($1, FTimes, $3) }
  | expr FDIVIDE expr                  { Binop($1, FDivide, $3) }
  | expr EQ      expr                  { Binop($1, Equal, $3) }
  | expr NEQ     expr                  { Binop($1, NEqual, $3) }
  | expr REQ     expr                  { Binop($1, RefEqual, $3) }
  | expr RNEQ    expr                  { Binop($1, RefNEqual, $3) }
  | expr LT      expr                  { Binop($1, Less, $3) }
  | expr LEQ     expr                  { Binop($1, Leq, $3) }
  | expr GT      expr                  { Binop($1, Greater, $3) }
  | expr GEQ     expr                  { Binop($1, Geq, $3) }
  | expr AND     expr                  { Binop($1, And, $3) }
  | expr OR      expr                  { Binop($1, Or, $3) }
  | MINUS expr %prec NEG               { Unop(Neg, $2) }
  | NOT          expr                  { Unop(Not, $2) }
  | expr ASSIGN  expr                  { Assign($1, $3) }
  | NEW TUPLE_TYP LPAREN actuals_opt RPAREN
                                       { Tuple($2, $4) }
  | NEW NODE_TYP LPAREN actuals_opt RPAREN
                                       { Node($2, $4) }
  | NEW NODE_TYP GRAPH LPAREN expr COMMA expr RPAREN
                                       { Graph($2, $5, $7) }
  | NEW typ LBRACKET actuals_opt RBRACKET
                                       { Lst($2, $4) }
  | RUN LBRACE expr RBRACE LPAREN actuals_opt RPAREN 
                                       { Run ($3,$6) }
  | ADD expr TO expr                   { ListAdd($2, $4) }
  | REMOVE expr FROM expr              { ListRemove($2, $4) }
  | ID LPAREN actuals_opt RPAREN       { Call($1, $3) }
  | LPAREN expr RPAREN                 { $2 }
  | NULL                               { Null }
  | INFINITY                           { Infinity }
  

actuals_opt:
    /* nothing */                      { [] }
  | actuals_list                       { List.rev $1 }

actuals_list:
    expr                               { [$1] }
  | actuals_list COMMA expr            { $3 :: $1 }
