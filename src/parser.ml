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
  let consume () = next () |> Helpers.blackhole in

  (* Try to match a token, consume it if successful *)
  let match_tok (lst : Tokens.token_type list) =
    match peak () with
    | Some { ty; _ } as tok -> begin
        match List.find_opt ((=) ty) lst with
        | Some _ -> consume (); tok
        | None -> None
        end
    | None -> None
  in

  (* atom_expr ::= I64
                 | L_PAREN expr R_PAREN ; *)
  let rec parse_atom_expr () =
    match next () with
    | Some { ty=(Tokens.I64 i); span } -> { expr=(I64 i); span }
    | Some { ty=L_PAREN; span } -> begin
        let { expr; _ } = parse_expr () in
        match next () with
        | Some { ty=R_PAREN; span=span_end } -> { expr; span=(merge_span span span_end) }
        | _ -> raise (SyntaxError { error=Unbalanced "paranthesis"; pos=span.left })
      end
    | _ -> raise (SyntaxError { error=Expecting "expression"; pos=(!lex_pos) })

  (* multiplication_expr ::= atom_expr (( STAR | SLASH ) atom_expr)* *)
  and parse_multiplication_expr () =
    let left = parse_atom_expr () in
    let rec aux expr =
      match match_tok [STAR; SLASH] with
      | Some { ty; span } -> aux { expr=Binary { op=(token2op ty); left=expr; right=(parse_atom_expr ()) }; span }
      | None -> expr
    in
    aux left

  (* addition_expr ::= multiplication_expr (( PLUS | MINUS ) multiplication_expr)* ; *)
  and parse_addition_expr () =
    let left = parse_multiplication_expr () in
    let rec aux expr =
      match match_tok [PLUS; MINUS] with
      | Some { ty; span } -> aux { expr=Binary { op=(token2op ty); left=expr; right=(parse_multiplication_expr ()) }; span }
      | None -> expr
    in
    aux left

  (* comparsion_expr ::= addition_expr (( GT | LT | GT_EQ | LT_EQ ) addition_expr)* ; *)
  and parse_comparsion_expr () =
    let left = parse_addition_expr () in
    let rec aux expr =
      match match_tok [GT; LT; GT_EQ; LT_EQ] with
      | Some { ty; span } -> aux { expr=Binary { op=(token2op ty); left=expr; right=(parse_addition_expr ()) }; span }
      | None -> expr
    in
    aux left

  (* equality_expr ::= comparsion_expr (( EQ | NOT_EQ ) comparsion_expr)* ; *)
  and parse_equality_expr () =
    let left = parse_comparsion_expr () in
    let rec aux expr =
      match match_tok [EQ; BANG_EQ] with
      | Some { ty; span } -> aux { expr=Binary { op=(token2op ty); left=expr; right=(parse_comparsion_expr ()) }; span }
      | _ -> expr
    in
    aux left

  (* expr ::= equality_expr *)
  and parse_expr () = parse_equality_expr () in

  try Ok (parse_expr ())
  with SyntaxError { error; pos } -> Error (error, pos)
