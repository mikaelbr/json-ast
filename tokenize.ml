open Token

let (<|) x f = x f;;
let (<<) f g x = f(g(x));;
let string_of_char = String.make 1;;
let string_of_char_list =
  List.fold_left (^) "" << List.map string_of_char;;
let rec list_char = function
  | "" -> []
  | ch ->
    let last_index = (String.length ch) - 1 in
    (String.get ch 0 ) :: (list_char (String.sub ch 1 last_index));;
let rec skip i = function
  | h :: t when i = 1 -> t
  | h :: t when i = 0 -> h :: t
  | h :: t -> skip (i - 1) t
  | _ -> [];;
let skip_str str = skip <| String.length str;;
let rec take i = function
  | h :: t -> if i = 0 then [] else h :: take (i - 1) t
  | _ -> [];;

type pos = { line: int; col: int; offset: int };;
type loc = { start: pos; stop: pos };;
let loc_skip i pos =
  { pos with col = pos.col + i; offset = pos.offset + i }
let loc_skip_line pos =
  { line = pos.line + 1; col = 0; offset = pos.offset + 1 }
let loc_skip_one = loc_skip 1
let loc_gen skip_loc_fn start =
  {
    start = start;
    stop = skip_loc_fn start
  }

let token_string cl source =
  let rec seek_string prev source =
    match source with
    | '"' :: t when prev <> '\\' -> ""
    | h :: t -> string_of_char h ^ seek_string h t
    | _ -> failwith "unreachable string" in
  let str = seek_string '"' source in
  let length = String.length str in
  (T_STRING str, loc_gen (loc_skip length) cl), skip length source;;

let token_number cl source =
  let rec seek_number = function
    | h :: t -> (match h with
        | '0'..'9' | 'e' | '.' | '+' | '-' ->
          string_of_char h ^ seek_number t
        | _ -> "")
    | _ -> failwith "unreachable number" in
  let str = seek_number source in
  let length = String.length str in
  (T_NUMBER str, loc_gen (loc_skip length) cl), skip length source;;

let token_sloc tt cl =
  (tt, loc_gen loc_skip_one cl)

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
  | 't' :: 'r' :: 'u' :: 'e' :: s -> (T_BOOL true, loc_gen (loc_skip 4) cl), s
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: s -> (T_BOOL false, loc_gen (loc_skip 5) cl), s
  | 'n' :: 'u' :: 'l' :: 'l' :: s -> (T_NULL, loc_gen (loc_skip 4) cl), s
  | '0' :: s -> (T_NUMBER "0", loc_gen loc_skip_one cl), s
  | '1'..'9' :: s -> token_number cl source
  | [] -> (T_EOF, loc_gen loc_skip_line cl), []
  | _ -> failwith ("unexpected token " ^ string_of_char_list source);;

let tokenize input =
  let rec tokenizeInput next_loc source =
    match token next_loc source with
    | ((c, l), []) -> [(c, l)]
    | ((c, l), source) -> (c, l) :: (tokenizeInput l.stop source) in
  tokenizeInput { line = 0; col = 0; offset = 0 } <| list_char input;;