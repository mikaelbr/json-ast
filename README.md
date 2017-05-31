# JSON AST generator for OCaml

Project currently mainly for learning OCaml. Work in progress.

```ml
(* Tokenize *)
get_content "test.json"
|> Tokenize.tokenize
|> Parser.parse
|> print_ast_value ;;
```
