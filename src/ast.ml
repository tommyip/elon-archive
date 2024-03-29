open Types

type variant_def =
  { name: string;
    variants: string spanned list
  }

let spanned_variant_def_str { x; _ } =
  let variants = String.concat " "
    (List.map (fun spanned -> spanned.x) x.variants) in
  Printf.sprintf "(variant %s %s)" x.name variants

type bin_op = Eq | NotEq | Gt | Lt | GtEq | LtEq | Add | Sub | Mul | Div | Mod | Exp | And | Or
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
  | AND -> And
  | OR -> Or
  | _ -> raise Helpers.Unreachable

let token2unaryop tok =
  let open Tokens in
  match tok with
  | NOT -> Not
  | MINUS -> Neg
  | _ -> raise Helpers.Unreachable

type literal
  = Unit
  | Bool of bool
  | I64 of Int64.t
  | F64 of float
  | Char of string
  | String of string

let literal_str = function
  | Unit -> "()"
  | Bool b -> Bool.to_string b
  | I64 i -> Int64.to_string i
  | F64 f -> Float.to_string f
  | Char c -> "'" ^ c ^ "'"
  | String s -> "\"" ^ s ^ "\""

type match_pattern
  = LiteralPat of literal
  | ConstructorPat of string
  | IdentPat of string

let spanned_match_pattern_str { x; _ } =
  match x with
  | LiteralPat lit -> literal_str lit
  | ConstructorPat con -> con
  | IdentPat id -> id

type expr
  = Let of { bind: string; value: expr spanned; expr: expr spanned }
  | Match of { target: string; cases: (match_pattern spanned * expr spanned) list }
  | Binary of { op: bin_op; left: expr spanned; right: expr spanned }
  | Unary of { op: unary_op; expr: expr spanned }
  | Literal of literal
  | Ident of string

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
  | And -> "And"
  | Or -> "Or"

let unary_op_str = function
  | Not -> "not"
  | Neg -> "-"

let rec spanned_expr_str expr =
  match expr.x with
  | Let { bind; value; expr } ->
      Printf.sprintf "(let (%s %s) %s)" bind (spanned_expr_str value) (spanned_expr_str expr)
  | Match { target; cases } ->
      let case_str (pat, expr) =
        Printf.sprintf "(%s %s)" (spanned_match_pattern_str pat) (spanned_expr_str expr )
      in
      Printf.sprintf "(match %s %s)" target (String.concat " " (List.map case_str cases ))
  | Binary { op; left; right } ->
      Printf.sprintf "(%s %s %s)" (bin_op_str op) (spanned_expr_str left) (spanned_expr_str right)
  | Unary { op; expr } ->
      Printf.sprintf "(%s %s)" (unary_op_str op) (spanned_expr_str expr)
  | Literal lit -> literal_str lit
  | Ident id -> id

