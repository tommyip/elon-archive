open Elon.Types
open Elon

module A = Alcotest

(* Helpers *)

let blackhole _ = ()

let lex_string string =
  let buf = Sedlexing.Utf8.from_string string in
  CCList.of_gen (fun () -> Lexer.token buf)

let token = A.testable token_pp ( = )

(* Tests *)

let test_keywords () =
  let src = "and or not let match variant mut" in
  let expected = [AND; OR; NOT; LET; MATCH; VARIANT; MUT] in
  A.(check (list token)) "Correct tokens" expected (lex_string src)

let test_operators () =
  let src = "= != > < >= <= + - * / mod ** :=" in
  let expected = [EQ; NOT_EQ; GT; LT; GTEQ; LTEQ; PLUS; MINUS; TIMES; DIVIDE;
                  MOD; EXP; WALRUS] in
  A.(check (list token)) "Correct tokens" expected (lex_string src)

let test_symbols () =
  let src = "( ) . .. ... : , -> => ; { } | [ ]" in
  let expected = [L_PAREN; R_PAREN; DOT; DOT_DOT; DOT_DOT_DOT; COLON; COMMA;
                  ARROW; FAT_ARROW; SEMICOLON; L_BRACKET; R_BRACKET; PIPE;
                  L_SQ_BRACKET; R_SQ_BRACKET] in
  A.(check (list token)) "Correct tokens" expected (lex_string src)

let test_literals_basics () =
  let src = "() true false" in
  let expected = [UNIT; BOOLEAN true; BOOLEAN false] in
  A.(check (list token)) "Correct tokens" expected (lex_string src)

let test_literals_char () =
  let src = "'a' 'A' '7' '火' 'Æ' 'ç' '🚀' '👨‍👩‍👧‍👦'" in
  let expected = [CHAR "a"; CHAR "A"; CHAR "7"; CHAR "火"; CHAR "Æ"; CHAR "ç";
                  CHAR "🚀"; CHAR "👨‍👩‍👧‍👦"] in
  A.(check (list token)) "Correct tokens" expected (lex_string src)

let test_literals_char_no_multiple_egc () =
  A.check_raises
    "A character contains a single extended grapheme cluster"
    (Lexer.CharLen "'ab'") (fun () -> lex_string "'ab'" |> blackhole)

let run () =
  A.run "Lexer" [
    "token", [
      A.test_case "Keywords" `Quick test_keywords;
      A.test_case "Operators" `Quick test_operators;
      A.test_case "Symbols" `Quick test_symbols;
      A.test_case "Literals - basics" `Quick test_literals_basics;
      A.test_case "Literals - char" `Quick test_literals_char;
      A.test_case "Literals - char (No multiple EGC)" `Quick test_literals_char_no_multiple_egc;
    ];
  ]
