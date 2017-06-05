open Token;;
open Alcotest;;

let (>>) f g x = g(f(x));;
let string_of_token_data data =
  (Json_ast.string_of_token data.token);;

let list_to_string = List.map string_of_token_data;;
let token_list_to_string = List.map Json_ast.string_of_token;;
let to_str = Json_ast.tokenize >> list_to_string;;

let inputs_and_expected = [
  "[]", [T_LBRACKET; T_RBRACKET];
  "{}", [T_LCURLY; T_RCURLY];
];;

let to_assert (input, expected) () =
  (check (list string)) input (token_list_to_string expected) (to_str input);;

let to_test x =
  fst x, `Quick , (to_assert x)

let tests = [
  "token tests", List.map to_test inputs_and_expected
];;