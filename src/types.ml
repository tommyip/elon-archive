type span =
  { left: int;
    right: int;
  }

type 'a spanned = { x: 'a; span: span }

let span_str { left; right } =
  if left = right then
    Printf.sprintf "position %d" left
  else
    Printf.sprintf "position %d to %d" left right

let span_pp fmt span =
  Format.pp_print_string fmt (span_str span)

let merge_span { left; _ } { right; _} = { left; right }

exception LexingError of string * span
exception ParsingError of string * span

type ctx =
  { path:    string;
  }
