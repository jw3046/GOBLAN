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
    Literal of int
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Tuple of TupleTyp * expr list
  | Node of NodeTyp * expr list
  | Graph of expr * expr
  | Lst of typ * expr list
  | Call of string * expr list
  | Noexpr
  | Null

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | For of expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue
  | Pass expr * expr
  | Run string * bind list

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

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | NEqual -> "!="
  | RefEqual -> "==="
  | RefNEqual -> "<>"
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"
  | _ -> "unknown_op" { raise (Failure  " [ERROR] Illegal string_of_op " )}



let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | _ -> "unknown_uop" { raise (Failure  " [ERROR] Illegal string_of_uop " )}



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
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | If(e1, e2, s1, s2, s3) -> "if (" ^ string_of_expr e1 ^ ")\n" ^ string_of_stmt s1 ^ "elif (" ^ string_of_expr e2 ^ ")\n" ^ string_of_stmt s2 ^ "else\n" ^ string_of_stmt s3
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | For (e1, e2, s) -> "for (" ^string_of_expr e1 ^ in string_of_expr e2 ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Pass (e1, e2) -> "Pass (" ^ string_of_tuple e1 ^ ") " ^ string_of_list e12
  | Run (e1, e2) -> "Run " ^ string_of_expr e1 ^ " ( " ^ string_of_list e2 ^ " ) "
  | _ -> "unknown_stmt" { raise (Failure  " [ERROR] Illegal string_of_stmt " )}
  
let string_of_n_data n_data = function
  "{\n" ^ String.concat ";\n" (Lisp.map string_of_attributes attributes) ";}\n"
  (******)

let string_of_n_do n_do = function
  "{\n" ^  String.concat ";\n" (List.map string_of_typ typ) ^ String.concat  "\n" 
  (****)

let string_of_n_catch var = function
 (****)

let string_of_node_decl node_decl = function
  (*****)
let string_of_func_decl func_decl = function
  (*****)
  
let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Char -> "char"
  | Str -> "string”
  | Float -> "float”
  | Graph -> "graph"
  | List -> "list"
  | Tuple -> "tuple"
  | ID -> "id"
  | _ -> "unknown_typ" { raise (Failure  " [ERROR] Illegal string_of_typ " )}

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_ndecl ///////////////placeholder??

let string_of_program (vars, funcs, nodes) =
  String.concat ""   (List.map string_of_vdecl vars)  ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_ndecl nodes)
