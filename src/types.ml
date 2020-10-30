type ctx =
  { path:    string;
    in_chan: in_channel;
  }

type number
  = I64 of Int64.t
  | F64 of float

type token
  (* Single character symbols *)
  = EQ
  | GT
  | LT
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | DOT
  | COLON
  | COMMA
  | SEMICOLON
  | PIPE
  | L_PAREN
  | R_PAREN
  | L_BRACKET
  | R_BRACKET
  | L_SQ_BRACKET
  | R_SQ_BRACKET
  (* Other symbols *)
  | BANG_EQ
  | GT_EQ
  | LT_EQ
  | STAR_STAR
  | DOT_DOT
  | ARROW
  | FAT_ARROW
  | COLON_EQ
  | DOT_DOT_DOT
  (* Keywords *)
  | AND
  | OR
  | NOT
  | LET
  | MATCH
  | VARIANT
  | RECORD
  | MUT
  (* Literals *)
  | UNIT
  | BOOLEAN of bool
  | NUMBER of number
  | CHAR of string
  | STRING of string
  (* Identifer *)
  | IDENT of string

let token_str = function
  | EQ -> "="
  | GT -> ">"
  | LT -> "<"
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | SLASH -> "/"
  | PERCENT -> "%"
  | DOT -> "."
  | COLON -> ":"
  | COMMA -> ","
  | SEMICOLON -> ";"
  | PIPE -> "|"
  | L_PAREN -> "("
  | R_PAREN -> ")"
  | L_BRACKET -> "{"
  | R_BRACKET -> "}"
  | L_SQ_BRACKET -> "["
  | R_SQ_BRACKET -> "]"
  | BANG_EQ -> "!="
  | GT_EQ -> "<="
  | LT_EQ -> ">="
  | STAR_STAR -> "**"
  | DOT_DOT -> ".."
  | ARROW -> "->"
  | FAT_ARROW -> "=>"
  | COLON_EQ -> ":="
  | DOT_DOT_DOT -> "..."
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | LET -> "LET"
  | MATCH -> "MATCH"
  | VARIANT -> "VARIANT"
  | RECORD -> "RECORD"
  | MUT -> "MUT"
  | UNIT -> "LITERAL ()"
  | BOOLEAN bool -> "LITERAL " ^ Bool.to_string bool
  | NUMBER (I64 i) -> "LITERAL " ^ Int64.to_string i
  | NUMBER (F64 f) -> "LITERAL " ^ Float.to_string f
  | CHAR char -> "LITERAL '" ^ char ^ "'"
  | STRING string -> "LITERAL \"" ^ string ^ "\""
  | IDENT ident -> "IDENT " ^ ident

let token_pp fmt tok =
  Format.pp_print_string fmt (token_str tok)
