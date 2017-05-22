open Printf
open Token
open Tokenize

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := (input_line chan ^ "\n") :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let get_content filename =
  read_file filename
  |> List.fold_left (^) "";;

let pos_to_string prefix pos =
  sprintf "(%s line: %d  col: %d offset: %d)" prefix pos.line pos.col pos.offset;;

let loc_to_string (loc: loc) =
  (pos_to_string "start" loc.start) ^ " - " ^ (pos_to_string "end" loc.stop) ;;

get_content "test.json"
|> tokenize
|> List.iter (fun (ct, loc) -> printf "%s, %s \n" (token_to_string ct) (loc_to_string loc))
