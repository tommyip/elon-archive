open Printf
open Elon.Types

type argument_error
  = NoArg
  | FileNotFound

let parse_arg () =
  if Array.length Sys.argv < 2 then
    Error(NoArg)
  else if not (Sys.file_exists Sys.argv.(1)) then
    Error(FileNotFound)
  else
    let path = Sys.argv.(1) in
    Ok({ path=path; in_chan=(open_in path) })

let () =
  match parse_arg () with
  | Ok ctx -> Elon.Driver.compile ctx
  | Error NoArg -> eprintf "Elon compiler\n\nUsage: elonc <file>\n"
  | Error FileNotFound -> eprintf "Error: File not found\n"
