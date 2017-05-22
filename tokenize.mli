type pos = { line: int; col: int; offset: int };;
type loc = { start: pos; stop: pos };;
val tokenize : string -> (Token.t * loc) list
val string_of_char_list : char list -> string