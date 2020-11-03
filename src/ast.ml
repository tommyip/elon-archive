type bin_op
  = Eq
  | NotEq
  | Gt
  | Lt
  | GtEq
  | LtEq
  | Add
  | Sub
  | Mul
  | Div

let token2op tok =
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
  | _ -> raise Helpers.Unreachable

type expr_inner
  = Binary of { op: bin_op; left: expr; right: expr }
  | I64 of Int64.t

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

let rec expr_str expr =
  let { expr; _ } = expr in
  match expr with
  | Binary { op; left; right } ->
      Printf.sprintf "(%s %s %s)" (bin_op_str op) (expr_str left) (expr_str right)
  | I64 i -> Int64.to_string i
