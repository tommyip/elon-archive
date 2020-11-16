open Elon.Types

let parse_args () =
  let path = ref "" in
  let set_path p = if !path = "" then path := p in
  let print_token_stream = ref false in
  let print_ast = ref false in
  let speclist = [
    ("--token-stream", Arg.Set print_token_stream, "Prints the token stream");
    ("--ast", Arg.Set print_ast, "Prints the abstract syntax tree");
  ] in
  let usage_msg = "Usage: elonc <file> [args...]" in
  Arg.parse speclist set_path usage_msg;
  {
    path=(!path);
    print_token_stream=(!print_token_stream);
    print_ast=(!print_ast);
  }

let () =
  let ctx = parse_args () in
  if Sys.file_exists ctx.path then
    Elon.Driver.compile ctx
  else
    Printf.eprintf "File not found\n"
