
(*type number =
  |	Int of int
  |	Big_int of Big_int.big_int
  | Float of float

  type number_type =
  | BINARY of number
  | OCTAL of number
  | NORMAL of number*)

type t =
    T_IDENTIFIER
  | T_LITERAL
  | T_OBJECT
  | T_ARRAY
  | T_NUMBER of string
  (*| T_NUMBER_EXPONENT of string*)
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
  (*| T_NUMBER_EXPONENT n -> "T_NUMBER " ^ n*)
  (*| T_NUMBER n -> let i = match n with
      | BINARY o -> "T_NUMBER BINARY " ^ string_of_num o
      | OCTAL o -> "T_NUMBER OCTAL " ^ string_of_num o
      | NORMAL o -> "T_NUMBER NORMAL " ^ string_of_num o in
    i*)
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
