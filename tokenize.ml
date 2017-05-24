open Batteries
open Token

let loc_skip i pos =
  { pos with col = pos.col + i;
             offset = pos.offset + i }
let loc_skip_line pos =
  { line = pos.line + 1;
    offset = pos.offset + 1;
    col = 0 }
let loc_skip_one = loc_skip 1

let loc_gen skip_loc_fn start =
  { start = start;
    stop = skip_loc_fn start}

let token_string cl source =
  let rec seek_string prev source =
    match source with
    | '"' :: t when prev <> '\\' -> ""
    | h :: t -> string_of_char h ^ seek_string h t
    | _ -> "" in
  let str = seek_string '"' source in
  let length = String.length str in
  (T_STRING str, loc_gen (loc_skip length) cl), skip length source;;

let token_number cl source =
  let rec seek_number = function
    | h :: t -> (match h with
        | '0'..'9' | 'e' | '.' | '+' | '-' ->
          string_of_char h ^ seek_number t
        | _ -> "")
    | [] -> "" in
  let str = seek_number source in
  let length = String.length str in
  (T_NUMBER str, loc_gen (loc_skip length) cl), skip length source;;

let token_sloc tt cl =
  (tt, loc_gen loc_skip_one cl);;

let token_sloc_s tt n cl =
  (tt, loc_gen (loc_skip n) cl);;

let rec token cl source = match source with
  | '[' :: s -> token_sloc T_LBRACKET cl, s
  | ']' :: s -> token_sloc T_RBRACKET cl, s
  | '{' :: s -> token_sloc T_LCURLY cl, s
  | '}' :: s -> token_sloc T_RCURLY cl, s
  | ',' :: s -> token_sloc T_COMMA cl, s
  | ':' :: s -> token_sloc T_COLON cl, s
  | '"' :: s -> token_string cl s
  | ' ' :: s -> token (loc_skip_one cl) s
  | '\n' :: s -> token (loc_skip_line cl) s
  | 't' :: 'r' :: 'u' :: 'e' :: s -> token_sloc_s (T_BOOL true) 4 cl, s
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: s -> token_sloc_s (T_BOOL false) 5 cl, s
  | 'n' :: 'u' :: 'l' :: 'l' :: s -> token_sloc_s T_NULL 4 cl, s
  | '0' :: s -> token_sloc (T_NUMBER "0") cl, s
  | '1'..'9' :: s -> token_number cl source
  | [] -> (T_EOF, loc_gen loc_skip_line cl), []
  | _ -> failwith "unexpected token";;


let to_token_data (t, loc) =
  { token = t ; loc }

let start_pos = { line = 0; col = 0; offset = 0 };;

let tokenize input =
  let rec tokenizeInput next_loc source =
    match token next_loc source with
    | (c, []) -> [c]
    | (c, source) -> c :: (tokenizeInput (snd c).stop source) in
  tokenizeInput start_pos
  <| list_char input
  |> List.map to_token_data;;