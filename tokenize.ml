open Token

let string_of_char = String.make 1
let string_of_char_list char_list =
  char_list
  |> List.map string_of_char
  |> List.fold_left (^) ""

let rec list_char ch = match ch with
  | "" -> []
  | ch -> (String.get ch 0 ) :: (list_char (String.sub ch 1 ( (String.length ch)-1) ) ) ;;

let rec skip i = function
  | h :: t -> if i = 0 then t else skip (i - 1) t
  | _ -> []

let rec take i = function
  | h :: t -> if i = 1 then [h] else [h] @ take (i - 1) t
  | _ -> []

let token_string start_quote source =
  let rec seek_string prev source =
    match source with
    | '"' :: t when prev <> '\\' -> "\"", t
    | h :: t ->
      let (a, b) = seek_string h t in
      string_of_char h ^ a, b
    | _ -> failwith "unreachable string" in
  let (str, source) = seek_string start_quote source in
  T_STRING ("\"" ^ str), source

let token_bool id source =
  let word = string_of_char_list (id :: take 3 source) in
  let v = match word with
    | "true" -> true
    | "false" -> false
    | _ -> failwith "unreachable bool" in
  T_BOOL v, skip 3 source

let token_null source =
  let word = string_of_char_list (take 3 source) in
  let v = match word with
    | "ull" -> T_NULL
    | _ -> failwith ("unreachable null " ^ word) in
  v, skip 3 source

let rec seek_number = function
  | h :: t ->
    let v = match h with
      | '0'..'9' | 'e' | '.' | '+' | '-' ->
        let (a, b) = seek_number t in
        string_of_char h ^ a, b
      | _ ->
        "", [h] @ t in
    v
  | _ -> failwith "unreachable number"
(*
let token_exponent base_number source =
  let seek_exponent (base, r) =
    let (a, source) = seek_number r in
    let number = base ^ a in
    T_NUMBER_EXPONENT number, source in
  let unpack_exponent b = function
    | s :: r when s = '+' || s = '-' ->
      b ^ string_of_char_list ['e'; s], r
    | r ->
      b ^ string_of_char_list ['e'], r in
  match source with
  | '0'..'9' :: f ->
    seek_exponent (unpack_exponent (string_of_char base_number) f)
  | 'e' :: f ->
    seek_exponent (unpack_exponent (string_of_char base_number) f)
  | _ -> failwith "unreachable exponent"*)

let token_number start_number source =
  (*match source with
    | h :: t when h = 'e' ->
    token_exponent start_number source
    | _ ->*)
  let (str, source) = seek_number source in
  let n = (string_of_char start_number ^ str) in
  T_NUMBER n, source

let rec token first source = match first with
  | '[' -> T_LBRACKET, source
  | ']' -> T_RBRACKET, source
  | '{' -> T_LCURLY, source
  | '}' -> T_RCURLY, source
  | ',' -> T_COMMA, source
  | ':' -> T_COLON, source
  | ' '
  | '\n' -> let n = match source with
      | h :: t -> token h t
      | _ -> failwith "unreachable whitespace" in
    n
  | '"' -> token_string first source
  | 't'
  | 'f' -> token_bool first source
  | 'n' -> token_null source
  | '1'..'9' -> token_number first source
  | _ -> failwith ("unexpected token " ^ string_of_char first)

let rec tokenizeInput (i: char list) = match i with
  | h :: t ->
    let (current, source) = token h t in
    [current] @ (tokenizeInput source)
  | _ ->  [ T_EOF ]

let tokenize input =
  tokenizeInput (list_char input)