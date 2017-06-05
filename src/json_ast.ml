open Batteries

let ast_of_json =
  Parser.parse << Tokenize.tokenize;;