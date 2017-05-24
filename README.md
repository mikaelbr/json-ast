# JSON AST generator for OCaml

Work in progress.

```ml
(* Tokenize *)
get_content "test.json"
|> tokenize
|> List.map string_of_token_data
|> List.iter print_endline
```