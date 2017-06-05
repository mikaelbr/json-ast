let tests =
  Token_test.tests @ Parser_test.tests;;

(* Run it *)
let () =
  Alcotest.run "ast of json tests" tests
