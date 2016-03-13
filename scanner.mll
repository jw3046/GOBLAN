(* Ocamllex scanner for “GOBLAN *)

{ open Parser }

Let exp = ('e'|'E')('+'|'-')?[0-9]+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }     (* Whitespace *)
| "/*"                 { comment lexbuf }   (* Comments *)
| "//"                 { slcomment lexbuf } (* Single Line Comment *)
| '#'                  { slcomment lexbuf } (* Single Line Comment *)
| '('                  { LPAREN }
| ')'                  { RPAREN }
| '{'                  { LBRACE }
| '}'                  { RBRACE }
| '['                  { LBRACKET }
| ']'                  { RBRACKET }
| '"'                  { string lexbuf }    (* String *)
| ';'                  { SEMI }
| ','                  { COMMA }
| '.'                  { PERIOD }
| '+'                  { PLUS }
| '-'                  { MINUS }
| '*'                  { TIMES }
| '/'                  { DIVIDE }
| '%'                  { MODULO }
| "+."                 { FPLUS }
| "-."                 { FMINUS }
| "*."                 { FTIMES }
| "/."                 { FDIVIDE }
| '='                  { ASSIGN }
| "=="                 { EQ }
| "!="                 { NEQ }
| "==="                { REQ }
| "<>"                 { RNEQ }
| '<'                  { LT }
| "<="                 { LEQ }
| ">"                  { GT }
| ">="                 { GEQ }
| "&&"                 { AND }
| "||"                 { OR }
| "!"                  { NOT }
| "if"                 { IF }
| "elif"               { ELLF }
| "else"               { ELSE }
| "for"                { FOR }
| "in"                 { IN }
| "while"              { WHILE }
| "break"              { BREAK }
| "continue"           { CONTINUE }
| "fun"                { FUNCTION }
| "return"             { RETURN }
| "bool"               { BOOL }
| "int"                { INT }
| "float"              { FLOAT }
| "char"               { CHAR }
| "string"             { STRING }
| "list"               { LIST }
| "tuple"              { TUPLE }
| "node"               { NODE }
| "graph"              { GRAPH }
| "edge"               { EDGE }
| "void"               { VOID }
| "true"               { TRUE }
| "false"              { FALSE }
| "data"               { DATA }
| "do"                 { DO }
| "catch"              { CATCH }
| "self"               { SELF }
| "prnt"               { PARENT }
| "chld"               { CHILD }
| "neighbors"          { NEIGHBORS }
| "message"            { MESSAGE }
| "pass"               { PASS }
| "to"                 { TO }
| "catch"              { CATCH }
| "add"                { ADD }
| "remove"             { REMOVE }
| "run"                { RUN }
| "null"               { NULL }
| "delete"             { DELETE }
| "from"               { FROM }
| "infinity"           { INFINITY }
| "main"               { MAIN }
| ('.'[0-9]+ exp?|[0-9]+('.'[0-9]* exp? | exp)) as float_lxm { LITERAL(float_of_string float_lxm) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and slcomment = parse
  '\n' { token lexbuf }
| _    { slcomment lexbuf }

and string = parse
  "\\\"" { string lexbuf } (* Double quote escape sequence: \" *)
| '"'    { token lexbuf }
| _      { string lexbuf }
