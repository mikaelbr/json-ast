open Printf
open Token
open Tokenize
open Ast_printer

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let get_content filename =
  read_file filename |> String.concat "\n"

let string_of_pos prefix pos =
  sprintf "(%s line: %d  col: %d offset: %d)" prefix pos.line pos.col pos.offset;;

let string_of_loc (loc: loc) =
  (string_of_pos "start" loc.start) ^ " - " ^ (string_of_pos "end" loc.stop) ;;

let string_of_token_data data =
  sprintf "%s, %s" (string_of_token data.token) (string_of_loc data.loc);;

let data = get_content "test.json"
           |> tokenize
           |> Parser.parse ;;

print_ast_value data

(*
|> List.map string_of_token_data
|> List.iter print_endline*)
