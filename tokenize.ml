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
  | h :: t -> if i = 0 then t else skip (i - 1) t
  | _ -> [];;
let skip_str str = skip <| String.length str;;
let rec take i = function
  | h :: t -> if i = 0 then [] else h :: take (i - 1) t
  | _ -> [];;


let token_string source =
  let rec seek_string prev source =
    match source with
    | '"' :: t when prev <> '\\' -> ""
    | h :: t -> string_of_char h ^ seek_string h t
    | _ -> failwith "unreachable string" in
  let str = seek_string '"' source in
  T_STRING str, skip_str str source;;

let token_number source =
  let rec seek_number = function
    | h :: t -> (match h with
        | '0'..'9' | 'e' | '.' | '+' | '-' ->
          string_of_char h ^ seek_number t
        | _ -> "")
    | _ -> failwith "unreachable number" in
  let str = seek_number source in
  T_NUMBER str, skip_str str source;;

let rec token source = match source with
  | '[' :: s -> T_LBRACKET, s
  | ']' :: s -> T_RBRACKET, s
  | '{' :: s -> T_LCURLY, s
  | '}' :: s -> T_RCURLY, s
  | ',' :: s -> T_COMMA, s
  | ':' :: s -> T_COLON, s
  | '"' :: s -> token_string s
  | (' ' | '\n') :: s -> token s
  | 't' :: 'r' :: 'u' :: 'e' :: s -> T_BOOL true, s
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: s -> T_BOOL false, s
  | 'n' :: 'u' :: 'l' :: 'l' :: s -> T_NULL, s
  | '0' :: s -> T_NUMBER "0", s
  | '1'..'9' :: s -> token_number source
  | _ -> failwith "unexpected token";;

let tokenize input =
  let rec tokenizeInput source =
    match token source with
    | (current, []) -> [current]
    | (current, source) -> current :: (tokenizeInput source) in
  tokenizeInput <| list_char input;;