open Printf
open Tokenize
open Token

let file = "test.json"

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
  read_file file
  |> List.fold_left (^) "" in

get_content file
|> tokenize
|> List.map token_to_string
|> List.iter print_endline
