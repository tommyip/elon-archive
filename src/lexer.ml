open Types
open Tokens

let utf8_len = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun len _ -> len + 1) 0

exception UnknownToken of Types.span
exception CharLen of string

let digit = [%sedlex.regexp? '0'..'9']
let integer = [%sedlex.regexp? Opt '-', Plus digit]
let float = [%sedlex.regexp? (integer, '.') | ('.', Plus digit) | (integer, '.', Plus digit)]
let lowercase = [%sedlex.regexp? 'a'..'z']
let uppercase = [%sedlex.regexp? 'A'..'Z']
let ident = [%sedlex.regexp? (lowercase | uppercase | '_'), Star (lowercase | uppercase | digit | '_')]

let rec token_ty buf =
  match%sedlex buf with
  | white_space -> token_ty buf
  | '=' -> Some EQ
  | '>' -> Some GT
  | '<' -> Some LT
  | '+' -> Some PLUS
  | '-' -> Some MINUS
  | '*' -> Some STAR
  | '/' -> Some SLASH
  | '%' -> Some PERCENT
  | '.' -> Some DOT
  | ':' -> Some COLON
  | ',' -> Some COMMA
  | ';' -> Some SEMICOLON
  | '|' -> Some PIPE
  | '(' -> Some L_PAREN
  | ')' -> Some R_PAREN
  | '{' -> Some L_BRACKET
  | '}' -> Some R_BRACKET
  | '[' -> Some L_SQ_BRACKET
  | ']' -> Some R_SQ_BRACKET
  | "!=" -> Some BANG_EQ
  | ">=" -> Some GT_EQ
  | "<=" -> Some LT_EQ
  | "**" -> Some STAR_STAR
  | ".." -> Some DOT_DOT
  | "->" -> Some ARROW
  | "=>" -> Some FAT_ARROW
  | ":=" -> Some COLON_EQ
  | "..." -> Some DOT_DOT_DOT
  | "and" -> Some AND
  | "or" -> Some OR
  | "not" -> Some NOT
  | "let" -> Some LET
  | "match" -> Some MATCH
  | "variant" -> Some VARIANT
  | "mut" -> Some MUT
  | "()" -> Some UNIT
  | "true" -> Some (BOOLEAN true)
  | "false" -> Some (BOOLEAN false)
  | '\'', any, Star Sub(any, '\''),'\'' ->
    (* A char is a unicode extended grapheme cluster which can contain
       multiple codepoints. *)
    let n_codepoints = Sedlexing.lexeme_length buf - 2 in
    let c = Sedlexing.Utf8.sub_lexeme buf 1 n_codepoints in
    if utf8_len c = 1 then
      Some (CHAR c)
    else
      raise (CharLen (Sedlexing.Utf8.lexeme buf))
  | '"', Star Sub(any, '"'), '"' ->
    let len = Sedlexing.lexeme_length buf - 2 in
    Some (STRING (Sedlexing.Utf8.sub_lexeme buf 1 len))
  | integer -> Some (I64 (Int64.of_string (Sedlexing.Utf8.lexeme buf)))
  | float -> Some (F64 (Float.of_string (Sedlexing.Utf8.lexeme buf)))
  | ident -> Some (IDENT (Sedlexing.Utf8.lexeme buf))
  | eof -> None
  | _ -> raise (UnknownToken { left=Sedlexing.lexeme_start buf; right=Sedlexing.lexeme_end buf })

let token buf =
  match (token_ty buf, Sedlexing.loc buf) with
  | Some ty, (left, right) -> Some { ty; span={ left; right } }
  | None, _ -> None

let rec tokens_pp buf =
  match token_ty buf with
  | Some tok -> print_endline (token_type_str tok); tokens_pp buf
  | None -> ()
