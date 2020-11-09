open Types
open Tokens
open Ast

exception SyntaxError of string * span

let syntax_error_pp error span =
  Printf.eprintf "[Syntax error]: %s at %s\n" error (span_str span)

(* binary_expr ::= lexpr ( tok1 | ... ) rexpr)* *)
let binary_op_parser matcher ~toks ~lparser ~rparser =
  let left = lparser () in
  let rec aux left =
    match matcher toks with
    | Some { ty; span } ->
        let op = token2binop ty in
        let right = rparser () in
        aux { node=Binary { op; left; right }; span }
    | None -> left
  in aux left

let parse token lexbuf =
  let tok_buf = ref None in
  let lex_pos = ref 0 in
  let lex_pos_span () =
    let pos = !lex_pos in
    { left=pos; right=pos }
  in
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

  let _binary_expr = binary_op_parser try_consume in

  let constructor_def () =
    match next () with
    | Some { ty=IDENT { name; is_capital }; span } ->
        if is_capital then
          { node=name; span }
        else
          raise (SyntaxError ("Variant constructor should be capitalized", span))
    | _ -> raise (SyntaxError ("Expecting a variant constructor", lex_pos_span ()))
  in

  let variant_def () =
    let start_span = (Option.get (next ())).span in
    match next () with
    | Some { ty=IDENT { name; is_capital=true }; _ } ->
        begin match try_consume [EQ] with
        | Some { ty=EQ; _ } ->
            let variant = constructor_def () in
            let rec aux variants =
              match try_consume [PIPE] with
              | Some _ -> aux (constructor_def () :: variants)
              | None -> variants
            in
            let variants = aux [variant] in
            let span = merge_span start_span (List.hd variants).span in
            { node={ name; variants }; span }
        | _ -> raise (SyntaxError ("Expecting a `=` symbol", lex_pos_span ()))
        end
    | Some { ty=IDENT { is_capital=false; _ }; span } ->
        raise (SyntaxError ("The type name of a variant should be capitalized", span))
    | _ ->
        let pos = start_span.right + 1 in
        let span = { left=pos; right=pos } in
        raise (SyntaxError ("Expecting a type name for the variant", span))
  in

  (*
  let rec atom_expr () =
    match next () with
    | Some { ty=UNIT; span } -> { node=Unit; span }
    | Some { ty=BOOLEAN b; span } -> { node=Bool b; span }
    | Some { ty=Tokens.I64 i; span } -> { node=I64 i; span }
    | Some { ty=Tokens.F64 f; span } -> { node=F64 f; span }
    | Some { ty=CHAR c; span } -> { node=Char c; span }
    | Some { ty=STRING c; span } -> { node=String c; span }
    | Some { ty=IDENT { name; _ }; span } -> { node=Ident name; span }
    | Some { ty=L_PAREN; span } -> begin
        let { node; _ } = expr () in
        match next () with
        | Some { ty=R_PAREN; span=span_end } -> { node; span=(merge_span span span_end) }
        | _ -> raise (SyntaxError { error=Unbalanced "paranthesis"; pos=span.left })
      end
    | _ -> raise (SyntaxError { error=Expecting "expression"; pos=(!lex_pos) })
  and unary_expr () =
    match try_consume [NOT; MINUS] with
    | Some { ty; span } ->
        let op = token2unaryop ty in
        let expr = atom_expr () in
        { node=Unary { op; expr }; span=(merge_span span expr.span) }
    | None -> atom_expr ()
  and exponentiation_expr () = binary_expr ~toks:[STAR_STAR] ~lparser:unary_expr ~rparser:exponentiation_expr
  and multiplication_expr () = binary_expr ~toks:[STAR; SLASH; PERCENT] ~lparser:exponentiation_expr ~rparser:exponentiation_expr
  and addition_expr () = binary_expr ~toks:[PLUS; MINUS] ~lparser:multiplication_expr ~rparser:multiplication_expr
  and comparsion_expr () = binary_expr ~toks:[GT; LT; GT_EQ; LT_EQ] ~lparser:addition_expr ~rparser:addition_expr
  and equality_expr () = binary_expr ~toks:[EQ; BANG_EQ] ~lparser:comparsion_expr ~rparser:comparsion_expr
  and and_expr () = binary_expr ~toks:[AND] ~lparser:equality_expr ~rparser:equality_expr
  and or_expr () = binary_expr ~toks:[OR] ~lparser:and_expr ~rparser:and_expr
  and expr () = or_expr () in
  *)

  let parse () =
    match peak () with
    | Some { ty=VARIANT; _ } -> variant_def ()
    | _ -> raise Helpers.Unreachable
  in

  try Ok (parse ())
  with SyntaxError (err, span) -> Error (err, span)
