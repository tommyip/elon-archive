open Tokens

type t

val init : Sedlexing.lexbuf -> t
(** [init lexbuf] Initialize a tokenizer state (ts) *)

val peak : t -> spanned_t
(** [peak ts] Return the next token in the token stream without advancing
    the stream.
    @raise LexingError if encountered an invalid token.
*)

val next : t -> spanned_t
(** [next ts] Return the next token in the token stream and advance the stream
    by one token.
    @raise LexingError if encountered an invalid token.
*)

val consume : t -> Tokens.token_id -> string -> spanned_t
(** [consume ts tok_id err_msg] Return the next token if it has the same id as
    [tok_id] and advance the token stream by one token.
    @raise ParsingError if the two id does not match.
*)

val consume_alts : t -> Tokens.token_id list -> (spanned_t, spanned_t) result
(** [consume_alts ts tok_ids] If the next token [tok]'s id is a member of
    [tok_ids] then advance the token stream and return [Ok tok], otherwise
    return [Error tok].
    @raise LexingError if encounted an invalid token.
*)

val print_token_stream : Sedlexing.lexbuf -> unit
