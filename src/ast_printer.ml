open Printf
open Ast_types
open Token
open Batteries

let string_of_actual_value = function
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | String x -> sprintf "\"%s\"" x
  | Bool x -> string_of_bool x;;

let string_of_ast_identifier (identifier : ast_identifier ) =
  sprintf "{
  \"type\": \"Identifier\",
  \"value\": \"%s\"
}" identifier.value

let rec string_of_ast_object_child ( child : ast_object_child ) =
  sprintf "{
  \"type\": \"Property\",
  \"key\": %s,
  \"value\": %s
}" (string_of_ast_identifier child.key) (string_of_ast child.value)

and string_of_ast_object (obj : ast_object) =
  sprintf "{
  \"type\": \"Object\",
  \"children\": [%s]
}" (obj.children
    |> List.map string_of_ast_object_child
    |> String.concat ",\n")

and string_of_ast_literal (literal : ast_literal) =
  sprintf "{
  \"type\": \"Literal\",
  \"value\": %s
}" (string_of_actual_value literal.raw_value)

and string_of_ast_array (arr : ast_array) =
  sprintf "{
  \"type\": \"Array\",
  \"children\": [%s]
}" (arr.children
    |> List.map string_of_ast
    |> String.concat ",\n")

and string_of_ast  = function
  | Literal literal -> string_of_ast_literal literal
  | Array arr -> string_of_ast_array arr
  | Object arr -> string_of_ast_object arr;;

let print_ast_value =
  print_string << string_of_ast
