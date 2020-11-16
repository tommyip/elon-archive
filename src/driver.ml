let compile ctx =
  let lexbuf () = Sedlexing.Utf8.from_channel (open_in Types.(ctx.path)) in
  if ctx.print_token_stream then
    Lexer.print_token_stream (lexbuf ());
  match Parser.parse (lexbuf ()) with
  | Ok expr ->
      if ctx.print_ast then
        print_endline (Ast.spanned_expr_str expr)
  | Error (err, pos) -> Parser.syntax_error_pp (err, pos)
