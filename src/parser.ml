open Types
open Tokens
open Ast

type syntax_error
  = Expecting of string
  | Unbalanced of string

exception SyntaxError of { error: syntax_error; pos: int }

let syntax_error_pp error pos =
  let msg = match error with
  | Expecting syn -> "Expecting " ^ syn
  | Unbalanced syn -> "Unbalanced " ^ syn
  in
  Printf.eprintf "[Syntax error]: %s at pos %d\n" msg pos

(* binary_expr ::= lexpr ( tok1 | ... ) rexpr)* *)
let binary_op_parser matcher ~toks ~lparser ~rparser =
  let left = lparser () in
  let rec aux left =
    match matcher toks with
    | Some { ty; span } ->
        let op = token2binop ty in
        let right = rparser () in
        aux { expr=Binary { op; left; right }; span }
    | None -> left
  in aux left

let parse token lexbuf =
  let tok_buf = ref None in
  let lex_pos = ref 0 in
  let peak () =
    if Option.is_none !tok_buf then
      tok_buf := token lexbuf;
    !tok_buf
  in
  let next () =
    match peak () with
    | Some Tokens.({ span; _ }) as tok -> begin
        tok_buf := None;
        lex_pos := span.right;
        tok
      end
    | None -> None
  in
  (* Try to match a token, consume and return it if successful *)
  let try_consume lst =
    match peak () with
    | Some { ty; _ } -> begin
        match List.find_opt ((=) ty) lst with
        | Some _ -> next ()
        | None -> None
        end
    | None -> None
  in

  let binary_expr = binary_op_parser try_consume in

  let rec atom_expr () =
    match next () with
    | Some { ty=UNIT; span } -> { expr=Unit; span }
    | Some { ty=BOOLEAN b; span } -> { expr=Bool b; span }
    | Some { ty=Tokens.I64 i; span } -> { expr=I64 i; span }
    | Some { ty=Tokens.F64 f; span } -> { expr=F64 f; span }
    | Some { ty=CHAR c; span } -> { expr=Char c; span }
    | Some { ty=STRING c; span } -> { expr=String c; span }
    | Some { ty=IDENT id; span } -> { expr=Ident id; span }
    | Some { ty=L_PAREN; span } -> begin
        let { expr; _ } = expr () in
        match next () with
        | Some { ty=R_PAREN; span=span_end } -> { expr; span=(merge_span span span_end) }
        | _ -> raise (SyntaxError { error=Unbalanced "paranthesis"; pos=span.left })
      end
    | _ -> raise (SyntaxError { error=Expecting "expression"; pos=(!lex_pos) })
  and unary_expr () =
    match try_consume [NOT; MINUS] with
    | Some { ty; span } ->
        let op = token2unaryop ty in
        let expr = atom_expr () in
        { expr=Unary { op; expr }; span=(merge_span span expr.span) }
    | None -> atom_expr ()
  and exponentiation_expr () = binary_expr ~toks:[STAR_STAR] ~lparser:unary_expr ~rparser:exponentiation_expr
  and multiplication_expr () = binary_expr ~toks:[STAR; SLASH; PERCENT] ~lparser:exponentiation_expr ~rparser:exponentiation_expr
  and addition_expr () = binary_expr ~toks:[PLUS; MINUS] ~lparser:multiplication_expr ~rparser:multiplication_expr
  and comparsion_expr () = binary_expr ~toks:[GT; LT; GT_EQ; LT_EQ] ~lparser:addition_expr ~rparser:addition_expr
  and equality_expr () = binary_expr ~toks:[EQ; BANG_EQ] ~lparser:comparsion_expr ~rparser:comparsion_expr
  and and_expr () = binary_expr ~toks:[AND] ~lparser:equality_expr ~rparser:equality_expr
  and or_expr () = binary_expr ~toks:[OR] ~lparser:and_expr ~rparser:and_expr
  and expr () = or_expr () in

  try Ok (expr ())
  with SyntaxError { error; pos } -> Error (error, pos)
