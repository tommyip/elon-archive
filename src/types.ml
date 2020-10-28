type ctx =
  { path:    string;
    in_chan: in_channel;
  }

type number
  = I64 of Int64.t
  | F64 of float

let number_pp = function
  | I64 i -> "i64 " ^ Int64.to_string i
  | F64 f -> "f64 " ^ Float.to_string f

type token
  = UNIT
  (* Booleans *)
  | BOOLEAN of bool
  | AND
  | OR
  | NOT
  (* Numbers *)
  | NUMBER of number
  | EQ
  | NOT_EQ
  | GT
  | LT
  | GTEQ
  | LTEQ
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | EXP
  | L_PAREN
  | R_PAREN
  (* Strings *)
  | CHAR of string
  | STRING of string
  (* Expressions *)
  | IDENT of string
  | DOT
  | DOT_DOT
  | DOT_DOT_DOT
  | COLON
  | COMMA
  | ARROW
  | FAT_ARROW
  | SEMICOLON
  | L_BRACKET
  | R_BRACKET
  | LET
  | MATCH
  | PIPE
  | WALRUS
  (* Structures *)
  | L_SQ_BRACKET
  | R_SQ_BRACKET
  | VARIANT
  | RECORD
  | MUT

let token_str = function
  | UNIT -> "()"
  | BOOLEAN true -> "TRUE"
  | BOOLEAN false -> "FALSE"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | NUMBER num -> "NUMBER " ^ number_pp num
  | EQ -> "="
  | NOT_EQ -> "!="
  | GT -> ">"
  | LT -> "<"
  | GTEQ -> ">="
  | LTEQ -> "<="
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIVIDE -> "/"
  | MOD -> "MOD"
  | EXP -> "**"
  | L_PAREN -> "("
  | R_PAREN -> ")"
  | CHAR c -> "CHAR " ^ c
  | STRING s -> "STRING " ^ s
  | IDENT ident -> "IDENT " ^ ident
  | DOT -> "."
  | DOT_DOT -> ".."
  | DOT_DOT_DOT -> "..."
  | COLON -> ":"
  | COMMA -> ","
  | ARROW -> "->"
  | FAT_ARROW -> "=>"
  | SEMICOLON -> ";"
  | L_BRACKET -> "{"
  | R_BRACKET -> "}"
  | LET -> "LET"
  | MATCH -> "MATCH"
  | PIPE -> "|"
  | WALRUS -> ":="
  | L_SQ_BRACKET -> "["
  | R_SQ_BRACKET -> "]"
  | VARIANT -> "VARIANT"
  | RECORD -> "RECORD"
  | MUT -> "MUT"

let token_pp fmt tok =
  Format.pp_print_string fmt (token_str tok)
