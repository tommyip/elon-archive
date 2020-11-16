open Elon
open Tokens
open Lexer
open Types
open Helpers

module A = Alcotest

(* Helpers *)

let lex_string string =
  let ts = Lexer.init (Sedlexing.Utf8.from_string string) in
  CCList.of_gen (fun () ->
    match next ts with
    | { x=EOF; _ } -> None
    | spanned_tok -> Some spanned_tok
  )

let token = A.testable spanned_t_pp ( = )
let token_ty = A.testable pp ( = )

let check_tokens ~src ~expected () =
  let tokens = List.map (fun { x; _ } -> x) (lex_string src) in
  A.check (A.list token_ty) "Correct tokens" expected tokens

(* Tests *)

let test_span () =
  let src = "var + 42 / 1337" in
  let expected = [
    { x=IDENT "var"; span={ left=0; right=3 } };
    { x=PLUS; span={ left=4; right=5 } };
    { x=I64 (Int64.of_int 42); span={ left=6; right=8 } };
    { x=SLASH; span={ left=9; right=10 } };
    { x=I64 (Int64.of_int 1337); span={ left=11; right=15 } }
  ] in
  A.check (A.list token) "Correct span information" expected (lex_string src)

let test_atom_symbols () =
  check_tokens ~src:"= < > + - * / % . : , ; | ( ) { } [ ]"
    ~expected:[EQ; LT; GT; PLUS; MINUS; STAR; SLASH; PERCENT; DOT; COLON;
               COMMA; SEMICOLON; PIPE; L_PAREN; R_PAREN; L_BRACKET; R_BRACKET;
               L_SQ_BRACKET; R_SQ_BRACKET] ()

let test_longer_symbols () =
  check_tokens ~src:"!= >= <= ** .. -> => := ..."
    ~expected:[BANG_EQ; GT_EQ; LT_EQ; STAR_STAR; DOT_DOT; ARROW; FAT_ARROW;
               COLON_EQ; DOT_DOT_DOT] ()

let test_keywords () =
  check_tokens ~src:"and or not let in match variant mut"
    ~expected:[AND; OR; NOT; LET; IN; MATCH; VARIANT; MUT] ()

let test_unit_and_boolean () =
  check_tokens ~src:"() true false"
    ~expected:[UNIT; BOOLEAN true; BOOLEAN false] ()

let test_integers () =
  let i64 i = I64 (Int64.of_int i) in
  check_tokens ~src:"0 1 123 9876543210 -0 -1 -123 -9876543210"
    ~expected:[i64 0; i64 1; i64 123; i64 9876543210;
               i64 0; i64 (-1); i64 (-123); i64 (-9876543210)] ()

let test_floats () =
  check_tokens ~src:"0. 123. .0 .123 123.321 1000.00 -0. -123. -987.654"
    ~expected:[F64 0.; F64 123.; F64 0.; F64 0.123; F64 123.321; F64 1000.;
               F64 (-0.); F64 (-123.); F64 (-987.654)] ()

let test_chars () =
  check_tokens ~src:"'a' 'A' '7' '火' 'Æ' 'ç' '🚀' '👨‍👩‍👧‍👦'"
    ~expected:[CHAR "a"; CHAR "A"; CHAR "7"; CHAR "火"; CHAR "Æ"; CHAR "ç";
               CHAR "🚀"; CHAR "👨‍👩‍👧‍👦"] ()

let test_chars_not_empty () =
  A.check_raises
    "Character cannot be empty"
    (LexingError ("Unknown token", { left=0; right=0 })) (fun () -> lex_string "''" |> blackhole)

let test_chars_no_multiple_egc () =
  A.check_raises
    "A character contains a single extended grapheme cluster"
    (LexingError ("This character literal contains more than one grapheme", { left=0; right=4 }))
    (fun () -> lex_string "'ab'" |> blackhole)

let test_strings () =
  check_tokens
    ~src:{|"" "a" "The spectacle before us was indeed sublime"
           "Hello, 世界!\n" "🚀->🪐"|}
    ~expected:[
      STRING "";
      STRING "a";
      STRING "The spectacle before us was indeed sublime";
      STRING "Hello, 世界!\\n";
      STRING "🚀->🪐"] ()

let test_idents () =
  check_tokens
    ~src:"x location sendLocation send_location SendLocation _ _x _SeNdLoCaTiOn"
    ~expected:[
      IDENT "x";
      IDENT "location";
      IDENT "sendLocation";
      IDENT "send_location";
      IDENT "SendLocation";
      IDENT "_";
      IDENT "_x";
      IDENT "_SeNdLoCaTiOn"] ()

let test_api_peak () =
  let ts = Lexer.init (Sedlexing.Utf8.from_string "42.") in
  let peak1 = Lexer.peak ts in
  A.check token "Peak returns next token" peak1 { x=F64 42.; span={ left=0; right=3 }};
  let peak2 = Lexer.peak ts in
  A.check token "Successive peaking does not advance token stream" peak2 peak1

let test_api_consume () =
  let ts = Lexer.init (Sedlexing.Utf8.from_string "42 + and") in
  let tok1 = Lexer.consume ts i64 "Expecting an integer" in
  A.check token "Returns the next token" tok1 { x=I64 (Int64.of_int 42); span={ left=0; right=2 } };
  let tok2 = Lexer.next ts in
  A.check (A.neg token) "And advance the token stream" tok2 tok1;
  A.check_raises "But don't advance the stream if token id does not match"
    (ParsingError ("Expecting an integer", { left=5; right=5 }))
    (fun () -> Lexer.consume ts i64 "Expecting an integer" |> blackhole)

let test_api_consume_alts () =
  let ts = Lexer.init (Sedlexing.Utf8.from_string "hello + world") in
  let tok1 = Result.get_ok (Lexer.consume_alts ts [pipe; ident]) in
  A.check token "Returns the next token" tok1 { x=IDENT "hello"; span={ left=0; right=5 } };
  let tok2 = Lexer.next ts in
  A.check (A.neg token) "And advance the token stream" tok2 tok1;
  let tok3 = Result.get_error (Lexer.consume_alts ts [f64]) in
  let tok4 = Lexer.next ts in
  A.check token "But don't advance the stream if not a member of toks" tok3 tok4

let run () =
  A.run "Lexer" [
    "token", [
      A.test_case "Token span" `Quick test_span;
      A.test_case "Atom symbols" `Quick test_atom_symbols;
      A.test_case "Longer symbols" `Quick test_longer_symbols;
      A.test_case "Keywords" `Quick test_keywords;
      A.test_case "Literals - unit and boolean" `Quick test_unit_and_boolean;
      A.test_case "Literals - integers" `Quick test_integers;
      A.test_case "Literals - floats" `Quick test_floats;
      A.test_case "Literals - chars" `Quick test_chars;
      A.test_case "Literals - chars (No empty char)" `Quick test_chars_not_empty;
      A.test_case "Literals - chars (No multiple EGC)" `Quick test_chars_no_multiple_egc;
      A.test_case "Literals - strings" `Quick test_strings;
      A.test_case "Identifiers" `Quick test_idents;
      A.test_case "API - peak" `Quick test_api_peak;
      A.test_case "API - consume" `Quick test_api_consume;
      A.test_case "API - consume_alts" `Quick test_api_consume_alts;
    ];
  ]
