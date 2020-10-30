open Elon.Types
open Elon

module A = Alcotest

(* Helpers *)

let blackhole _ = ()

let lex_string string =
  let buf = Sedlexing.Utf8.from_string string in
  CCList.of_gen (fun () -> Lexer.token buf)

let token = A.testable token_pp ( = )

let check_tokens ~src ~expected () =
  A.check (A.list token) "Correct tokens" expected (lex_string src)

(* Tests *)

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
  check_tokens ~src:"and or not let match variant mut"
    ~expected:[AND; OR; NOT; LET; MATCH; VARIANT; MUT] ()

let test_unit_and_boolean () =
  check_tokens ~src:"() true false"
    ~expected:[UNIT; BOOLEAN true; BOOLEAN false] ()

let test_integers () =
  let i64 i = NUMBER (I64 (Int64.of_int i)) in
  check_tokens ~src:"0 1 123 9876543210 -0 -1 -123 -9876543210"
    ~expected:[i64 0; i64 1; i64 123; i64 9876543210;
               i64 0; i64 (-1); i64 (-123); i64 (-9876543210)] ()

let test_floats () =
  let f64 f = NUMBER (F64 f) in
  check_tokens ~src:"0. 123. .0 .123 123.321 1000.00 -0. -123. -987.654"
    ~expected:[f64 0.; f64 123.; f64 0.; f64 0.123; f64 123.321; f64 1000.;
               f64 (-0.); f64 (-123.); f64 (-987.654)] ()

let test_chars () =
  check_tokens ~src:"'a' 'A' '7' '火' 'Æ' 'ç' '🚀' '👨‍👩‍👧‍👦'"
    ~expected:[CHAR "a"; CHAR "A"; CHAR "7"; CHAR "火"; CHAR "Æ"; CHAR "ç";
               CHAR "🚀"; CHAR "👨‍👩‍👧‍👦"] ()

let test_chars_not_empty () =
  A.check_raises
    "Character cannot be empty"
    (Lexer.UnknownToken (0, 0)) (fun () -> lex_string "''" |> blackhole)

let test_chars_no_multiple_egc () =
  A.check_raises
    "A character contains a single extended grapheme cluster"
    (Lexer.CharLen "'ab'") (fun () -> lex_string "'ab'" |> blackhole)

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

let run () =
  A.run "Lexer" [
    "token", [
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
    ];
  ]
