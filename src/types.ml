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

let merge_span { left; _ } { right; _} = { left; right }

type ctx =
  { path:    string;
  }
