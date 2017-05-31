# JSON AST generator for OCaml

Project currently mainly for learning OCaml. Work in progress.

```ml
(* Tokenize *)
get_content "test.json"
|> tokenize
|> List.map string_of_token_data
|> List.iter print_endline
```
