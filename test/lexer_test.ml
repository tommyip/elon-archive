open Elon.Types
open Elon

module A = Alcotest

let lex_string string =
  let buf = Sedlexing.Utf8.from_string string in
  CCList.of_gen (fun () -> Lexer.token buf)

let token = A.testable token_pp ( = )

let test_keywords () =
  let src = "and or not mod let match variant mut" in
  let expected = [AND; OR; NOT; MOD; LET; MATCH; VARIANT; MUT] in
  A.(check (list token)) "Correct tokens" expected (lex_string src)

let run () =
  A.run "Lexer" [
    "token", [
      A.test_case "Keywords" `Quick test_keywords;
    ];
  ]
