open Printf
open Ast_types
open Token
open Batteries

let string_of_actual_value = function
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | String x -> sprintf "\"%s\"" x
  | Bool x -> string_of_bool x;;

let string_of_ast_literal (literal : ast_literal) =
  sprintf "{ type: \"Literal\", value: %s }"
    (string_of_actual_value literal.raw_value)

let string_of_ast  = function
  | Literal literal -> string_of_ast_literal literal
  | _ -> failwith "Incomplete print";;

let print_ast_value =
  print_string << string_of_ast
