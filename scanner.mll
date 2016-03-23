(* Ocamllex scanner for GOBLAN *)

{ open Parser }

let exp = ('e'|'E')('+'|'-')?['0'-'9']+;

rule token = parse
  [' ' '\t' '\r' '\n']                 { token lexbuf }     (* Whitespace *)
| "/*"                                 { comment lexbuf }   (* Comments *)
| "//"                                 { slcomment lexbuf } (* Comments *)
| '#'                                  { slcomment lexbuf } (* Comments *)
| '('                                  { LPAREN }
| ')'                                  { RPAREN }
| '{'                                  { LBRACE }
| '}'                                  { RBRACE }
| '['                                  { LBRACKET }
| ']'                                  { RBRACKET }
| ';'                                  { SEMI }
| ','                                  { COMMA }
| '.'                                  { PERIOD }
| '+'                                  { PLUS }
| '-'                                  { MINUS }
| '*'                                  { TIMES }
| '/'                                  { DIVIDE }
| '%'                                  { MODULO }
| "+."                                 { FPLUS }
| "-."                                 { FMINUS }
| "*."                                 { FTIMES }
| "/."                                 { FDIVIDE }
| '='                                  { ASSIGN }
| "=="                                 { EQ }
| "!="                                 { NEQ }
| "==="                                { REQ }
| "<>"                                 { RNEQ }
| '<'                                  { LT }
| "<="                                 { LEQ }
| ">"                                  { GT }
| ">="                                 { GEQ }
| "&&"                                 { AND }
| "||"                                 { OR }
| "!"                                  { NOT }
| "->"                                 { ARROW }
| "if"                                 { IF }
| "elif"                               { ELIF }
| "else"                               { ELSE }
| "for"                                { FOR }
| "in"                                 { IN }
| "while"                              { WHILE }
| "break"                              { BREAK }
| "continue"                           { CONTINUE }
| "return"                             { RETURN }
| "bool"                               { BOOL }
| "int"                                { INT }
| "float"                              { FLOAT }
| "char"                               { CHAR }
| "string"                             { STRING }
| "list"                               { LIST }
| "tuple"                              { TUPLE }
| "node"                               { NODE }
| "graph"                              { GRAPH }
| "new"                                { NEW }
| "void"                               { VOID }
| "true"                               { TRUE }
| "false"                              { FALSE }
| "data"                               { DATA }
| "do"                                 { DO }
| "catch"                              { CATCH }
| "self"                               { SELF }
| "prnt"                               { PARENT }
| "chld"                               { CHILD }
| "neighbors"                          { NEIGHBORS }
| "message"                            { MESSAGE }
| "pass"                               { PASS }
| "add"                                { ADD }
| "to"                                 { TO }
| "remove"                             { REMOVE }
| "from"                               { FROM }
| "run"                                { RUN }
| "null"                               { NULL }
| "infinity"                           { INFINITY }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm
                                       { ID(lxm) }
| ('.'[0-9]+ exp?|[0-9]+('.'[0-9]* exp? | exp)) as lxm
                                       { FLT_LIT(float_of_string lxm) }
| ['0'-'9']+ as lxm                    { INT_LIT(int_of_string lxm) }
| '"'[_|"\\\""]*'"'                    { let string = Lexing.lexeme lexbuf in
                                         STR_LIT(String.sub string 1 (
                                         (String.length string) - 2))
| "node:"['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm
                                       { NODE_TYP(lxm) }
| "tuple:"['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm
                                       { TUPLE_TYP(lxm) }
| eof                                  { EOF }
| _ as char                            { raise (Failure("illegal character " ^
                                         Char.escaped char))
                                       }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and slcomment = parse
  '\n' { token lexbuf }
| _    { slcomment lexbuf }

