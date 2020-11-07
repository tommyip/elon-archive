type bin_op = Eq | NotEq | Gt | Lt | GtEq | LtEq | Add | Sub | Mul | Div | Mod | Exp
type unary_op = Not | Neg

let token2binop tok =
  let open Tokens in
  match tok with
  | EQ -> Eq
  | BANG_EQ -> NotEq
  | GT -> Gt
  | LT -> Lt
  | GT_EQ -> GtEq
  | LT_EQ -> LtEq
  | PLUS -> Add
  | MINUS -> Sub
  | STAR -> Mul
  | SLASH -> Div
  | PERCENT -> Mod
  | STAR_STAR -> Exp
  | _ -> raise Helpers.Unreachable

let token2unaryop tok =
  let open Tokens in
  match tok with
  | NOT -> Not
  | MINUS -> Neg
  | _ -> raise Helpers.Unreachable

type expr_inner
  = Binary of { op: bin_op; left: expr; right: expr }
  | Unary of { op: unary_op; expr: expr }
  | Unit
  | Bool of bool
  | I64 of Int64.t
  | F64 of float
  | Char of string
  | String of string
  | Ident of string

and expr = { expr: expr_inner; span: Types.span }

let bin_op_str = function
  | Eq -> "="
  | NotEq -> "!="
  | Gt -> ">"
  | Lt -> "<"
  | GtEq -> ">="
  | LtEq -> "<="
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Exp -> "**"

let unary_op_str = function
  | Not -> "not"
  | Neg -> "-"

let rec expr_str expr =
  let { expr; _ } = expr in
  match expr with
  | Binary { op; left; right } ->
      Printf.sprintf "(%s %s %s)" (bin_op_str op) (expr_str left) (expr_str right)
  | Unary { op; expr } ->
      Printf.sprintf "(%s %s)" (unary_op_str op) (expr_str expr)
  | Unit -> "()"
  | Bool b -> Bool.to_string b
  | I64 i -> Int64.to_string i
  | F64 f -> Float.to_string f
  | Char c -> "'" ^ c ^ "'"
  | String s -> "\"" ^ s ^ "\""
  | Ident id -> id

