let compile ctx =
  let expr =
    open_in Types.(ctx.path)
    |> Sedlexing.Utf8.from_channel
    |> Parser.parse
  in
  match expr with
  | Ok expr -> print_endline (Ast.spanned_expr_str expr)
  | Error (err, pos) -> Parser.syntax_error_pp (err, pos)
