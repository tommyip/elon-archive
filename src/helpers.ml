exception Unreachable

let blackhole _ = ()

let is_capital word =
  let first_char = String.get word 0 in
  first_char >= 'A' && first_char <= 'Z'

let span_at_pos pos =
  Types.({ left=pos; right=pos })
