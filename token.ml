type t =
    T_IDENTIFIER
  | T_LITERAL
  | T_OBJECT
  | T_ARRAY
  | T_NUMBER of string
  | T_STRING of string
  | T_BOOL of bool
  | T_COMMA
  | T_COLON
  | T_LCURLY
  | T_RCURLY
  | T_LBRACKET
  | T_RBRACKET
  | T_NULL
  | T_EOF

let token_to_string = function
  | T_NUMBER n -> "T_NUMBER " ^ n
  | T_STRING x -> "T_STRING " ^ x
  | T_BOOL b -> "T_BOOL " ^ string_of_bool b
  | T_IDENTIFIER -> "T_IDENTIFIER"
  | T_LITERAL -> "T_LITERAL"
  | T_OBJECT -> "T_OBJECT"
  | T_ARRAY -> "T_ARRAY"
  | T_COMMA -> "T_COMMA"
  | T_COLON -> "T_COLON"
  | T_LCURLY -> "T_LCURLY"
  | T_RCURLY -> "T_RCURLY"
  | T_LBRACKET -> "T_LBRACKET"
  | T_RBRACKET -> "T_RBRACKET"
  | T_NULL -> "T_NULL"
  | T_EOF -> "T_EOF"
