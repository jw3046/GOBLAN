(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | FAdd | FSub | FMult | FDiv |
          Equal | NEqual | RefEqual | RefNEqual| Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = 
    Int | Bool | Void | Str | Float 
  | NodeTyp of string 
  | GraphTyp of string
  | ListTyp of typ
  | TupleTyp of string

type bind = typ * string

type expr =
    Member of expr * string
  | IntLit of int
  | BoolLit of bool
  | StrLit of string
  | FloatLit of float
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Tuple of string * expr list
  | Node of string * expr list
  | Graph of string * expr * expr
  | Lst of typ * expr list
  | Call of string * expr list
  | Run of expr * expr list
  | ListAdd of expr * expr
  | ListRemove of expr * expr
  | Neighbors
  | Parent
  | Child
  | Self
  | Message
  | Null
  | Infinity
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | ReturnNoExpr
  | Return of expr
  | If of expr * stmt * stmt 
  | For of expr * expr * expr * stmt
  | ForEach of expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue
  | Pass of expr * expr

type tuple_decl = {
    typ : string;
    attributes : bind list;
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
    n_typ  : string;
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

let rec string_of_typ = function
    Int            -> "int"
  | Bool           -> "bool"
  | Void           -> "void"
  | Str            -> "string"
  | Float          -> "float"
  | NodeTyp(name)  -> name
  | GraphTyp(name) -> name
  | ListTyp(name)  -> string_of_typ name
  | TupleTyp(name) -> name

let string_of_op = function
    Add       -> "+"
  | Sub       -> "-"
  | Mult      -> "*"
  | Div       -> "/"
  | FAdd      -> "+."
  | FSub      -> "-."
  | FMult     -> "*."
  | FDiv      -> "/."
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

let string_of_bind_list (typ, id) =
  string_of_typ typ ^ " " ^ id

let rec string_of_expr = function
  | Member(e, id) -> string_of_expr e ^ "." ^ id
  | IntLit(i) -> string_of_int i
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StrLit(s) -> s
  | FloatLit(f) -> string_of_float f
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Tuple(typ, el) -> typ ^ " " ^
      String.concat "\n" (List.map string_of_expr el)
  | Node(typ, el) -> typ ^ " " ^
      String.concat "\n" (List.map string_of_expr el)
  | Graph(typ, e1, e2) -> typ ^ "\n" ^ string_of_expr e1 ^ "\n" ^
      string_of_expr e2
  | Lst(typ, el) -> string_of_typ typ ^ " " ^
      String.concat "\n" (List.map string_of_expr el)
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Run(e, bl) -> string_of_expr e ^ " " ^ String.concat "\n" (List.map string_of_expr bl)
  | ListAdd(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
  | ListRemove(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
  | Neighbors -> "neighbors"
  | Parent -> "parent"
  | Child -> "child"
  | Self -> "self"
  | Message -> "message"
  | Null -> "null"
  | Infinity -> "infinity"
  | Noexpr -> ""

let rec string_of_stmt = function
    Expr(e)
      -> string_of_expr e ^ ";"
  | Block(stmts)
      -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}"
  | ReturnNoExpr
      -> "return;"
  | Return(expr)
      -> "return " ^ string_of_expr expr ^ ";"
  | If(e, s1, s2)
      -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^
         "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s)
      -> "for (" ^ string_of_expr e1  ^ "; " ^ string_of_expr e2 ^ "; " ^
         string_of_expr e3  ^ ")\n" ^ string_of_stmt s
  | ForEach(e1, e2, s)
      -> "for (" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")\n" ^
         string_of_stmt s
  | While(e, s)
      -> "while (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | Break
      -> "break;"
  | Continue
      -> "continue;"
  | Pass (e1, e2)
      -> "Pass (" ^ string_of_expr e1 ^ ")\n" ^ string_of_expr e2
 
let string_of_v_decl (v_decl_typ, v_decl_id) = 
  string_of_typ v_decl_typ ^ " " ^ v_decl_id

let string_of_formals formals =
  String.concat ", " (List.map string_of_v_decl formals)

let string_of_locals locals = 
  String.concat ";\n  " (List.map string_of_v_decl locals)

let string_of_body body =
  String.concat "\n  " (List.map string_of_stmt body)

let string_of_f_decl f_decl =
  string_of_typ f_decl.typ ^ " " ^ f_decl.fname ^
    "(" ^ string_of_formals f_decl.formals ^ ") {\n  " ^
    string_of_locals f_decl.locals ^ ";\n  " ^
    string_of_body f_decl.body ^ "\n}"

let string_of_data ndata =
  String.concat "\n" (List.map string_of_v_decl ndata.attributes)

let string_of_do (ndo : n_do) =
  string_of_typ ndo.typ ^ "\n" ^
  string_of_formals ndo.formals ^ "\n" ^
  string_of_locals ndo.locals ^ "\n" ^
  string_of_body ndo.body

let string_of_catch (ncatch : n_catch) =
  string_of_locals ncatch.locals ^ "\n" ^
  string_of_body ncatch.body

let string_of_n_decl n_decl =
  n_decl.n_typ ^ " {\n" ^
  string_of_data n_decl.n_data ^ "\n" ^
  "}{\n" ^
  string_of_do n_decl.n_do ^ "\n" ^
  "}{\n" ^
  string_of_catch n_decl.n_catch ^ "\n" ^
  "}"

let string_of_attributes attr =
  String.concat "\n" (List.map string_of_bind_list attr)

let string_of_t_decl (t_decl : tuple_decl) =
  t_decl.typ ^ "\n" ^ string_of_attributes t_decl.attributes

let string_of_program (vars, tuples, nodes, funcs) =
  String.concat "\n" (List.map string_of_v_decl vars)  ^ "\n" ^
  String.concat "\n" (List.map string_of_f_decl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_n_decl nodes) ^ "\n" ^
  String.concat "\n" (List.map string_of_t_decl tuples)
