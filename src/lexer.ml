open Types
open Tokens

let utf8_len = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun len _ -> len + 1) 0

exception LexingError of string * span

let digit = [%sedlex.regexp? '0'..'9']
let integer = [%sedlex.regexp? Opt '-', Plus digit]
let float = [%sedlex.regexp? (integer, '.') | ('.', Plus digit) | (integer, '.', Plus digit)]
let lowercase = [%sedlex.regexp? 'a'..'z']
let uppercase = [%sedlex.regexp? 'A'..'Z']
let ident = [%sedlex.regexp? (lowercase | uppercase | '_'), Star (lowercase | uppercase | digit | '_')]

let lexspan lexbuf = { left=Sedlexing.lexeme_start lexbuf; right=Sedlexing.lexeme_end lexbuf }

let rec token_ty buf =
  match%sedlex buf with
  | white_space -> token_ty buf
  | '=' -> EQ
  | '>' -> GT
  | '<' -> LT
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> STAR
  | '/' -> SLASH
  | '%' -> PERCENT
  | '.' -> DOT
  | ':' -> COLON
  | ',' -> COMMA
  | ';' -> SEMICOLON
  | '|' -> PIPE
  | '(' -> L_PAREN
  | ')' -> R_PAREN
  | '{' -> L_BRACKET
  | '}' -> R_BRACKET
  | '[' -> L_SQ_BRACKET
  | ']' -> R_SQ_BRACKET
  | "!=" -> BANG_EQ
  | ">=" -> GT_EQ
  | "<=" -> LT_EQ
  | "**" -> STAR_STAR
  | ".." -> DOT_DOT
  | "->" -> ARROW
  | "=>" -> FAT_ARROW
  | ":=" -> COLON_EQ
  | "..." -> DOT_DOT_DOT
  | "and" -> AND
  | "or" -> OR
  | "not" -> NOT
  | "let" -> LET
  | "match" -> MATCH
  | "variant" -> VARIANT
  | "mut" -> MUT
  | "()" -> UNIT
  | "true" -> BOOLEAN true
  | "false" -> BOOLEAN false
  | '\'', any, Star Sub(any, '\''),'\'' ->
    (* A char is a unicode extended grapheme cluster which can contain
       multiple codepoints. *)
    let n_codepoints = Sedlexing.lexeme_length buf - 2 in
    let c = Sedlexing.Utf8.sub_lexeme buf 1 n_codepoints in
    if utf8_len c = 1 then CHAR c
    else raise (LexingError ("This character literal contains more than one grapheme", lexspan buf))
  | '"', Star Sub(any, '"'), '"' ->
    let len = Sedlexing.lexeme_length buf - 2 in
    STRING (Sedlexing.Utf8.sub_lexeme buf 1 len)
  | integer -> I64 (Int64.of_string (Sedlexing.Utf8.lexeme buf))
  | float -> F64 (Float.of_string (Sedlexing.Utf8.lexeme buf))
  | ident -> IDENT (Sedlexing.Utf8.lexeme buf)
  | eof -> EOF
  | _ -> raise (LexingError ("Unknown token", lexspan buf))

let token buf =
  let x = token_ty buf in
  let span = lexspan buf in
  { x; span }

(* Tokenizer state *)
type t =
  { lexbuf: Sedlexing.lexbuf;
    tokbuf: Tokens.t spanned option ref;
  }

let init lexbuf =
  { lexbuf;
    tokbuf=ref None;
  }

let peak state =
  match !(state.tokbuf) with
  | Some tok -> tok
  | None ->
      let tok = token state.lexbuf in
      state.tokbuf := Some tok;
      tok

let next state =
  match peak state with
  | { x=EOF; _ } as eof -> eof
  | tok -> state.tokbuf := None; tok

let consume state toks =
  let spanned = peak state in
  match List.find_opt ((=) (Variants.to_rank spanned.x)) toks with
  | Some _ ->
      if spanned.x != EOF then state.tokbuf := None;
      Ok spanned
  | None -> Error spanned

let rec print_token_stream buf =
  match token_ty buf with
  | EOF -> ()
  | tok -> print_endline (token_type_str tok); print_token_stream buf
