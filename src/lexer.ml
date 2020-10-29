open Types

let utf8_len = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun len _ -> len + 1) 0

exception UnknownToken of int * int
exception CharLen of string

let digit = [%sedlex.regexp? '0'..'9']
let integer = [%sedlex.regexp? Opt '-', Plus digit]
let float = [%sedlex.regexp? (integer, '.') | ('.', Plus digit) | (integer, '.', Plus digit)]
let lowercase = [%sedlex.regexp? 'a'..'z']
let uppercase = [%sedlex.regexp? 'A'..'Z']
let ident = [%sedlex.regexp? (lowercase | uppercase | '_'), Star (lowercase | uppercase | digit | '_')]

let rec token buf =
  match%sedlex buf with
  | white_space -> token buf
  | "()" -> Some UNIT
  | "true" -> Some (BOOLEAN true)
  | "false" -> Some (BOOLEAN false)
  | "and" -> Some AND
  | "or" -> Some OR
  | "not" -> Some NOT
  | '=' -> Some EQ
  | "!=" -> Some NOT_EQ
  | '>' -> Some GT
  | '<' -> Some LT
  | ">=" -> Some GTEQ
  | "<=" -> Some LTEQ
  | '+' -> Some PLUS
  | '-' -> Some MINUS
  | '*' -> Some TIMES
  | '/' -> Some DIVIDE
  | "mod" -> Some MOD
  | "**" -> Some EXP
  | '(' -> Some L_PAREN
  | ')' -> Some R_PAREN
  | '.' -> Some DOT
  | ".." -> Some DOT_DOT
  | "..." -> Some DOT_DOT_DOT
  | ':' -> Some COLON
  | ',' -> Some COMMA
  | "->" -> Some ARROW
  | "=>" -> Some FAT_ARROW
  | ';' -> Some SEMICOLON
  | '{' -> Some L_BRACKET
  | '}' -> Some R_BRACKET
  | "let" -> Some LET
  | "match" -> Some MATCH
  | '|' -> Some PIPE
  | ":=" -> Some WALRUS
  | '[' -> Some L_SQ_BRACKET
  | ']' -> Some R_SQ_BRACKET
  | "variant" -> Some VARIANT
  | "mut" -> Some MUT
  | '\'', any, Star Sub(any, '\''),'\'' ->
    (* A char is a unicode extended grapheme cluster which can contain
       multiple codepoints. *)
    let n_codepoints = Sedlexing.lexeme_length buf - 2 in
    let c = Sedlexing.Utf8.sub_lexeme buf 1 n_codepoints in
    if utf8_len c = 1 then
      Some (CHAR c)
    else
      raise (CharLen (Sedlexing.Utf8.lexeme buf))
  | '"', Star Sub(any, '"'), '"' -> Some (STRING (Sedlexing.Utf8.lexeme buf))
  | integer -> Some (NUMBER (I64 (Int64.of_string (Sedlexing.Utf8.lexeme buf))))
  | float -> Some (NUMBER (F64 (Float.of_string (Sedlexing.Utf8.lexeme buf))))
  | ident -> Some (IDENT (Sedlexing.Utf8.lexeme buf))
  | eof -> None
  | _ -> raise (UnknownToken (Sedlexing.lexeme_start buf, Sedlexing.lexeme_end buf))

let rec tokens_pp buf =
  match token buf with
  | Some tok -> print_endline (token_str tok); tokens_pp buf
  | None -> ()

let lex ctx =
  let lexbuf = Sedlexing.Utf8.from_channel ctx.in_chan in
  tokens_pp lexbuf
