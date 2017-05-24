open Token
open Ast_types

let to_literal current =
  match current.token with
  | T_NUMBER raw ->
    { _type = current.token;
      raw_value = String raw;
      value = Int (int_of_string raw);
      loc = current.loc }
  | T_STRING raw ->
    { _type = current.token;
      raw_value = String raw;
      value = String raw;
      loc = current.loc  }
  | T_BOOL raw ->
    { _type = current.token;
      raw_value = Bool raw;
      value = Bool raw;
      loc = current.loc  }
  | _ -> failwith "unexpected literal";;

let walk current rest =
  match current.token with
  | T_NUMBER _
  | T_BOOL _
  | T_STRING _ -> Literal (to_literal current)
  | _ -> failwith ("unexpected token " ^ string_of_token current.token)

let parse (tokens : token_data list): ast_value =
  match tokens with
  | h :: r -> walk h r
  | [] -> failwith "unexpected token"
