/* Ocamlyacc parser for GOBLAN */

%{
open Ast;
let get1 (a,_,_) = a;
let get2 (_,a,_) = a;
let get3 (_,_,a) = a;
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMI COMMA PERIOD PLUS MINUS TIMES DIVIDE MODULO
%token FPLUS FMINUS FTMES FDIVIDE ASSIGN EQ NEQ REQ RNEQ
%token LT LEQ GT GEQ AND OR NOT IF ELIF ELSE FOR IN WHILE
%token BREAK CONTINUE FUNCTION RETURN BOOL INT FLOAT CHAR
%token STRING LIST TUPLE NODE GRAPH VOID TRUE FALSE
%token DATA DO CATCH SELF PARENT CHILD NEIGHBORS MESSAGE
%token PASS TO ADD RUN NULL DELETE FROM INFINITY
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
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
  decls EOF { $1 }

decls:
   /* nothing */ { [], [], [] }
 | decls vdecl { ($2 :: get1 $1), get2 $1, get3 $1 }
 | decls fdecl { get1 $1, ($2 :: get2 $1), get3 $1 }
 | decls ndecl { get1 $1, get2 $1, ($2 :: get3 $1) }

ndecl:
   NODE ID LBRACE n_data n_do n_catch RBRACE
     { { n_data = $4;
        n_do = $5;
        n_catch = $6 } }

n_data:
   DATA LBRACE vdecl_list RBRACE
     { { attributes = List.rev $7} }

n_do:
   typ DO LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
        formals = $4;
        locals = List.rev $7;
        body = List.rev $8 } }

n_catch:
   CATCH LBRACE vdecl_list vdecl_list stmt_list RBRACE
     { { locals = $4;
        body = $5 } }


fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT      { Int }
  | BOOL     { Bool }
  | VOID     { Void }
  | CHAR     { Char}
  | STRING   { Str }
  | FLOAT    { Float }
  | ID GRAPH { Graph }
  | typ LIST { List }
  | ID       { Tuple }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt_block:
    LBRACE stmt_list RBRACE { List.rev $2 }

stmt_brace:
    LBRACE stmt RBRACE { Block ($2) }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | stmt_block { Block($1) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt_brace %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt_brace ELSE stmt_brace   { If($3, $5, $7) }
  | IF LPAREN expr RPAREN stmt_brace ELIF stmt_brace ELSE stmt_brace { If ($3, $5, $7, $9) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt_brace { For($3, $5, $7, $9) }
  | FOR LPAREN expr IN expr RPAREN stmt_brace { For($3, $5, $7)}
  | WHILE LPAREN expr RPAREN stmt_brace { While($3, $5) }
  | BREAK SEMI { Break }
  | CONTINUE SEMI { Continue }
  | SEMI { Empty }
  | PASS expr ARROW ID SEMI { Pass($2, $4) }
  | RUN expr LPAREN expr RPAREN { Run ($2, $4) }
  | expr COMMA expr { Comma ($1, $3) }
  

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL           { Literal($1) }
  | TRUE              { BoolLit(true) }
  | FALSE             { BoolLit(false) }
  | ID                { Id($1) }
  | expr PLUS   expr  { Binop($1, Add,        $3) }
  | expr MINUS  expr  { Binop($1, Sub,        $3) }
  | expr TIMES  expr  { Binop($1, Mult,       $3) }
  | expr DIVIDE expr  { Binop($1, Div,        $3) }
  | expr MODULO expr  { Binop($1, Mod,        $3) }
  | expr FPLUS  expr  { Binop($1, FAdd,       $3) }
  | expr FMINUS expr  { Binop($1, FMinus,     $3) }
  | expr FTIMES expr  { Binop($1, FTimes,     $3) }
  | expr FDIVIDE expr { Binop($1, FDivide,    $3) }
  | expr EQ     expr  { Binop($1, Equal,      $3) }
  | expr NEQ    expr  { Binop($1, NEqual,     $3) }
  | expr REQ    expr  { Binop($1, RefEqual,   $3) }
  | expr NREQ   expr  { Binop($1, NRefEqual,  $3) }
  | expr LT     expr  { Binop($1, Less,       $3) }
  | expr LEQ    expr  { Binop($1, Leq,        $3) }
  | expr GT     expr  { Binop($1, Greater,    $3) }
  | expr GEQ    expr  { Binop($1, Geq,        $3) }
  | expr AND    expr  { Binop($1, And,        $3) }
  | expr OR     expr  { Binop($1, Or,         $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr          { Unop(Not, $2) }
  | ID ASSIGN expr    { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | NULL { Null }
  | INFINITY { Infinity }
  

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
