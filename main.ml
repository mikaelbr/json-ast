open Printf
open Token

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
  read_file filename
  |> List.fold_left (^) "";;

get_content "test.json"
|> Tokenize.tokenize
|> List.map token_to_string
|> List.iter print_endline
