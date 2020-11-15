open Types
open Tokens
open Lexer
open Ast

let syntax_error_pp (error, span) =
  Printf.eprintf "[Syntax error]: %s at %s\n" error (span_str span)

(* A generic rule for binary expressions
   binary_expr ::= lexpr ( tok1 | ... ) rexpr)* *)
let binary_expr ts toks ~lparser ~rparser =
  let left = lparser ts in
  let rec aux left =
    match consume_alts ts toks with
    | Ok { x; span } ->
        let op = token2binop x in
        let right = rparser ts in
        aux { x=Binary { op; left; right }; span }
    | Error _ -> left
  in aux left

let match_pattern ts =
  match next ts with
  | { x=UNIT; span } -> { x=LiteralPat Unit; span }
  | { x=BOOLEAN b; span } -> { x=LiteralPat (Bool b); span }
  | { x=Tokens.I64 i; span } -> { x=LiteralPat (I64 i); span }
  | { x=Tokens.F64 f; span } -> { x=LiteralPat (F64 f); span }
  | { x=CHAR c; span } -> { x=LiteralPat (Char c); span }
  | { x=STRING c; span } -> { x=LiteralPat (String c); span }
  | { x=IDENT ctor; span } when Helpers.is_capital ctor ->
      { x=ConstructorPat ctor; span }
  | { x=IDENT var; span } -> { x=IdentPat var; span }
  | { span; _ } -> raise (ParsingError ("Expecting a pattern here", span))

let rec match_expr ts =
  let { span=lspan; _ } = next ts in
  match next ts with
  | { x=IDENT target; _ } when Bool.not (Helpers.is_capital target) ->
      consume ts l_bracket "Expecting a `{` here" |> Helpers.blackhole;
      let cases = CCList.of_gen (fun () ->
        consume_alts ts [pipe]
        |> Result.to_option
        |> Option.map (fun _ ->
          let pat = match_pattern ts in
          consume ts arrow "Expecting a `->` here" |> Helpers.blackhole;
          (pat, expr ts)
        )
      ) in
      let { span=rspan; _ } = consume ts r_bracket "Expecting a `}` here" in
      let span = merge_span lspan rspan in
      if List.length cases > 0 then
          { x=Match { target; cases }; span }
      else
        raise (ParsingError ("A match expression must have at least one match arm", span))
  | { span; _ } -> raise (ParsingError ("Expecting an identifier here", span))

and atom ts =
  match consume_alts ts [unit; boolean; i64; f64; char; string; ident; l_paren] with
  | Ok { x=UNIT; span } -> { x=Literal Unit; span }
  | Ok { x=BOOLEAN b; span } -> { x=Literal (Bool b); span }
  | Ok { x=Tokens.I64 i; span } -> { x=Literal (I64 i); span }
  | Ok { x=Tokens.F64 f; span } -> { x=Literal (F64 f); span }
  | Ok { x=CHAR c; span } -> { x=Literal (Char c); span }
  | Ok { x=STRING c; span } -> { x=Literal (String c); span }
  | Ok { x=IDENT name; span } -> { x=Ident name; span }
  | Ok { x=L_PAREN; span=lspan } -> begin
      let { x; _ } = expr ts in
      match next ts with
      | { x=R_PAREN; span=rspan } -> { x; span=(merge_span lspan rspan) }
      | _ -> raise (ParsingError ("It looks like this paranthesis is not closed", lspan))
    end
  | Error { x=MATCH; _ } -> match_expr ts
  | Error { span; _ } -> raise (ParsingError ("Expecting an expression here", span))
  | _ -> raise Helpers.Unreachable

and unary_expr ts =
  match consume_alts ts [not; minus] with
  | Ok { x; span } ->
      let op = token2unaryop x in
      let expr = atom ts in
      { x=Unary { op; expr }; span=(merge_span span expr.span) }
  | Error _ -> atom ts

and exponentiation_expr ts =
  binary_expr ts [star_star] ~lparser:unary_expr ~rparser:exponentiation_expr

and multiplication_expr ts =
  binary_expr ts [star; slash; percent] ~lparser:exponentiation_expr ~rparser:exponentiation_expr

and addition_expr ts =
  binary_expr ts [plus; minus] ~lparser:multiplication_expr ~rparser:multiplication_expr

and comparsion_expr ts =
  binary_expr ts [gt; lt; gt_eq; lt_eq] ~lparser:addition_expr ~rparser:addition_expr

and equality_expr ts =
  binary_expr ts [eq; bang_eq] ~lparser:comparsion_expr ~rparser:comparsion_expr

and and_expr ts =
  binary_expr ts [and_] ~lparser:equality_expr ~rparser:equality_expr

and or_expr ts =
  binary_expr ts [or_] ~lparser:and_expr ~rparser:and_expr

and expr ts = or_expr ts

let parse lexbuf =
  try
    let ast = expr (Lexer.init lexbuf) in
    Ok(ast)
  with
    | ParsingError (err, span) -> Error (err, span)
    | LexingError (err, span) -> Error (err, span)
