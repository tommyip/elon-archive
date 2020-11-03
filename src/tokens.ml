type token_type
  = EOF
  (* Single character symbols *)
  | EQ
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
  | I64 of Int64.t
  | F64 of float
  | CHAR of string
  | STRING of string
  (* Identifer *)
  | IDENT of string

type token = { ty: token_type; span: Types.span }

let token_type_str = function
  | EOF -> "EOF"
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
  | I64 i -> "LITERAL " ^ Int64.to_string i
  | F64 f -> "LITERAL " ^ Float.to_string f
  | CHAR char -> "LITERAL '" ^ char ^ "'"
  | STRING string -> "LITERAL \"" ^ string ^ "\""
  | IDENT ident -> "IDENT " ^ ident

let token_ty_pp fmt token_ty =
  Format.pp_print_string fmt (token_type_str token_ty)

let token_pp fmt { ty; span } =
  Format.fprintf fmt "{ ty=%s; span={ left=%d; right=%d } }" (token_type_str ty) span.left span.right
