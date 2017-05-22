type pos = { line: int; col: int; offset: int };;
type loc = { start: pos; stop: pos };;
type token_data = { token : Token.t; loc: loc }
val tokenize : string -> token_data list
