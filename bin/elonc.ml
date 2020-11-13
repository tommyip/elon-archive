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
    Ok({ path=Sys.argv.(1) })

let () =
  match parse_arg () with
  | Ok ctx -> Elon.Driver.compile ctx
  | Error NoArg -> eprintf "Elon compiler\n\nUsage: elonc <file>\n"
  | Error FileNotFound -> eprintf "Error: File not found\n"
