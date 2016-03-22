(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | FAdd | FSub | FMult | FDiv |
          Equal | NEqual | RefEqual | RefNEqual| Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = 
    Int | Bool | Void | Char | Str | Float 
  | NodeTyp of string 
  | GraphTyp of string
  | ListTyp of string
  | TupleTyp of string

type bind = typ * string

type expr =
    Member of expr * expr
  | IntLit of int
  | BoolLit of bool
  | StrLit of string
  | FloatLit of float
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Tuple of typ * expr list
  | Node of typ * expr list
  | Graph of typ * expr * expr
  | Lst of typ * expr list
  | Call of string * expr list
  | Run of expr * bind list
  | ListAdd of expr * expr
  | ListRemove of expr * expr
  | Neighbors
  | Parent
  | Child
  | Self
  | Mssage
  | Null
  | Infinity
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | ForEach of expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue
  | Pass of expr * expr

type tuple_decl = {
    attributes : expr list;
  }

type n_data = {
    attributes : bind list;
  }

type n_do = {
    typ : typ;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type n_catch = {
    locals : bind list;
    body : stmt list;
  }

type node_decl = {
    n_data : n_data;
    n_do : n_do;
    n_catch : n_catch;
  }

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * tuple_decl list * node_decl list * func_decl list

(* Pretty-printing functions *)

let string_of_typ = function
    Int   -> "int"
  | Bool  -> "bool"
  | Void  -> "void"
  | Char  -> "char"
  | Str   -> "string"
  | Float -> "float"
  | NodeTyp(name) -> "node"
  | GraphTyp(name) -> "graph"
  | ListTyp(name) -> "list"
  | TupleTyp(name) -> "tuple"

let string_of_op = function
    Add       -> "+"
  | Sub       -> "-"
  | Mult      -> "*"
  | Div       -> "/"
  | Equal     -> "=="
  | NEqual    -> "!="
  | RefEqual  -> "==="
  | RefNEqual -> "<>"
  | Less      -> "<"
  | Leq       -> "<="
  | Greater   -> ">"
  | Geq       -> ">="
  | And       -> "&&"
  | Or        -> "||"
  | Mod       -> "%"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Null -> "Null"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts)
      -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)
      -> string_of_expr expr ^ ";\n"
  | Return(expr)
      -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, Block([]))
      -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2)
      -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1
         "else\n" ^ string_of_stmt s2
  | If(e1, e2, s1, s2, s3)
      -> "if (" ^ string_of_expr e1 ^ ")\n" ^ string_of_stmt s1 ^
         "elif (" ^ string_of_expr e2 ^ ")\n" ^ string_of_stmt s2 ^
         "else\n" ^ string_of_stmt s3
  | For(e1, e2, e3, s)
      -> "for (" ^ string_of_expr e1  ^ "; " ^ string_of_expr e2 ^ "; " ^
         string_of_expr e3  ^ ")\n" ^ string_of_stmt s
  | For (e1, e2, s)
      -> "for (" ^ string_of_expr e1 ^ string_of_expr e2 ^ ")\n" ^
         string_of_stmt s
  | While(e, s)
      -> "while (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | Pass (e1, e2)
      -> "Pass (" ^ string_of_tuple e1 ^ ")\n" ^ string_of_list e2
  | Run (e1, e2)
      -> "Run " ^ string_of_expr e1 ^ " ( " ^ string_of_list e2 ^ " ) "
 
let string_of_v_decl (v_decl_typ, v_decl_id) = 
  string_of_typ v_decl_typ ^ " " ^ v_decl_id

let string_of_formals formals =
  String.concat "\n" (List.map string_of_v_decl formals)

let string_of_locals locals = 
  Strng.concat "\n" (List.map string_of_v_decl locals)

let string_of_body body =
  String.concat "\n" (List.map string_of_stmt body)

let string_of_f_decl f_decl =
  string_of_typ f_decl.typ ^ " " ^ f_decl.fname ^
    "(" ^ string_of_formals f_decl.formals ^ ")\n" ^
    string_of_locals f_decl.locals ^ "\n" ^
    string_of_body f_decl.body

let string_of_data ndata =
  String.concat "\n" (List.map string_of_v_decl ndata.attributes)

let string_of_do ndo =
  string_of_typ n_do.typ ^ "\n" ^
  string_of_formals n_do.formals ^ "\n" ^
  string_of_locals n_do.locals ^ "\n" ^
  string_of_body n_do.body

let string_of_catch ncatch =
  string_of_locals ncatch.locals ^ "\n" ^
  string_of_body ncatch.body

let string_of_n_decl n_decl =
  string_of_typ n_decl.n_typ ^ " {\n" ^
  string_of_data n_decl.n_data ^ "\n" ^
  "}{\n" ^
  string_of_do n_decl.n_do ^ "\n" ^
  "}{\n" ^
  string_of_catch n_decl.n_catch ^ "\n" ^
  "}"

let string_of_tdecl t_decl =
  string_of_typ t_decl.typ ^ " " ^ 
  String.concat "\n" (List.map string_of_vdecl t_decl.attributes)

(* tuples ?*)
let string_of_program (vars, funcs, nodes, tuples) =
  String.concat "\n" (List.map string_of_vdecl vars)  ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_ndecl nodes) ^ "\n" ^
  String.concat "\n" (List.map string_of_tdecl tuples)
