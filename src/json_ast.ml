open Batteries

let ast_of_json =
  Parser.parse << Tokenize.tokenize;;

let parse = Parser.parse;;
let tokenize = Tokenize.tokenize;;

let string_of_token = Token.string_of_token;;