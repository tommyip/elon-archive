open Types

type t
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
  | IN
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
  [@@deriving variants]

type token_id = int
let tid higher_order_variant : token_id = higher_order_variant.Variantslib.Variant.rank

open Variants
let eof = tid eof
let eq = tid eq
let gt = tid gt
let lt = tid lt
let plus = tid plus
let minus = tid minus
let star = tid star
let slash = tid slash
let percent = tid percent
let dot = tid dot
let colon = tid colon
let comma = tid comma
let semicolon = tid semicolon
let pipe = tid pipe
let l_paren = tid l_paren
let r_paren = tid r_paren
let l_bracket = tid l_bracket
let r_bracket = tid r_bracket
let l_sq_bracket = tid l_sq_bracket
let r_sq_bracket = tid r_sq_bracket
let bang_eq = tid bang_eq
let gt_eq = tid gt_eq
let lt_eq = tid lt_eq
let star_star = tid star_star
let dot_dot = tid dot_dot
let arrow = tid arrow
let fat_arrow = tid fat_arrow
let colon_eq = tid colon_eq
let dot_dot_dot = tid dot_dot_dot
let and_ = tid and_
let or_ = tid or_
let not = tid not
let let_ = tid let_
let in_ = tid in_
let match_ = tid match_
let variant = tid variant
let record = tid record
let mut = tid mut
let unit = tid unit
let boolean = tid boolean
let i64 = tid i64
let f64 = tid f64
let char = tid char
let string = tid string
let ident = tid ident

type spanned_t = t Types.spanned

let token_type_str = function
  | EOF -> "EOF"
  | EQ -> "SYMBOL ="
  | GT -> "SYMBOL >"
  | LT -> "SYMBOL <"
  | PLUS -> "SYMBOL +"
  | MINUS -> "SYMBOL -"
  | STAR -> "SYMBOL *"
  | SLASH -> "SYMBOL /"
  | PERCENT -> "SYMBOL %"
  | DOT -> "SYMBOL ."
  | COLON -> "SYMBOL :"
  | COMMA -> "SYMBOL ,"
  | SEMICOLON -> "SYMBOL ;"
  | PIPE -> "SYMBOL |"
  | L_PAREN -> "SYMBOL ("
  | R_PAREN -> "SYMBOL )"
  | L_BRACKET -> "SYMBOL {"
  | R_BRACKET -> "SYMBOL }"
  | L_SQ_BRACKET -> "SYMBOL ["
  | R_SQ_BRACKET -> "SYMBOL ]"
  | BANG_EQ -> "SYMBOL !="
  | GT_EQ -> "SYMBOL <="
  | LT_EQ -> "SYMBOL >="
  | STAR_STAR -> "SYMBOL **"
  | DOT_DOT -> "SYMBOL .."
  | ARROW -> "SYMBOL ->"
  | FAT_ARROW -> "SYMBOL =>"
  | COLON_EQ -> "SYMBOL :="
  | DOT_DOT_DOT -> "SYMBOL ..."
  | AND -> "SYMBOL and"
  | OR -> "SYMBOL or"
  | NOT -> "SYMBOL not"
  | LET -> "KEYWORD let"
  | IN -> "KEYWORD in"
  | MATCH -> "KEYWORD match"
  | VARIANT -> "KEYWORD variant"
  | RECORD -> "KEYWORD record"
  | MUT -> "KEYWORD mut"
  | UNIT -> "LITERAL ()"
  | BOOLEAN bool -> "LITERAL " ^ Bool.to_string bool
  | I64 i -> "LITERAL " ^ Int64.to_string i
  | F64 f -> "LITERAL " ^ Float.to_string f
  | CHAR char -> "LITERAL '" ^ char ^ "'"
  | STRING string -> "LITERAL \"" ^ string ^ "\""
  | IDENT name -> "IDENT " ^ name

let pp fmt token_ty =
  Format.pp_print_string fmt (token_type_str token_ty)

let spanned_t_pp fmt { x; span } =
  Format.fprintf fmt "{ tok=%s; span={ left=%d; right=%d } }" (token_type_str x) span.left span.right
