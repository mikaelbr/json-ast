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