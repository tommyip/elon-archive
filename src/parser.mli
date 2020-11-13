open Types

val parse : Sedlexing.lexbuf -> (Ast.expr spanned, string * span) result

val syntax_error_pp : string * span -> unit
