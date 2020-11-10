let compile ctx =
  let lexbuf = Sedlexing.Utf8.from_channel Types.(ctx.in_chan) in
  let expr = Parser.parse Lexer.token lexbuf in
  match expr with
  | Ok expr -> print_endline (Ast.spanned_expr_str expr)
  | Error (err, pos) -> Parser.syntax_error_pp err pos
