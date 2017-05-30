open Token
open Ast_types

let get_value = function
  | T_NUMBER raw -> String raw, Int (int_of_string raw)
  | T_STRING raw -> String raw, String raw
  | T_BOOL raw -> Bool raw, Bool raw
  | _ -> failwith "unexpected literal"

let to_literal current =
  let (raw, value) = get_value current.token in
  { _type = current.token;
    raw_value = raw;
    value = value;
    loc = current.loc }

let prop key_string key_loc value : ast_object_child =
  { _type = T_PROPERTY;
    key = { _type = T_IDENTIFIER;
            value = key_string;
            loc = key_loc };
    value = value;
    loc = key_loc };;

let failtoken token_data =
  failwith ("unexpected token " ^ string_of_token token_data.token)

let rec to_array (current : token_data) rest : ast_array * 'a =
  let parent : ast_array = { _type = T_ARRAY;
                             loc = current.loc;
                             children = []} in
  let (children, r) = match rest with
    | h :: r ->
      (match h.token with
       | T_RBRACKET -> [], r
       | _ -> walk h (Some (Array parent)) r )
    | _ -> failwith "unreachable" in
  { parent with children = children }, r

and to_property parent = function
  | { token = T_STRING x; loc } :: { token = T_COLON } :: h :: r ->
    let (c, r) = (walk h parent r) in
    prop x loc (List.hd c), r
  | { token = T_STRING _ } :: r :: _ -> failtoken r
  | r :: _ -> failtoken r
  | _ -> failwith "Unexpected end"

and property_children parent_object rest =
  let (child, rest) = to_property parent_object rest in
  match rest with
  | [] -> [child], []
  | h :: r ->
    (match h.token with
     | T_COMMA ->
       let (c, rest) = property_children parent_object r in
       child :: c, rest
     | T_RCURLY -> [child], r
     | _ -> failwith "unreachable")
  | _ -> failwith "unreachable"

and to_object (current : token_data) rest : ast_object * 'a =
  let parent : ast_object = { _type = T_OBJECT;
                              loc = current.loc;
                              children = []} in
  let parent_object = Some (Object parent) in
  let (children, rest) = (match rest with
      | { token = T_RCURLY } :: r -> [], r
      | _ -> property_children parent_object rest) in
  { parent with children = children }, rest

and walk current (parent : ast_value option) rest =
  match current.token with
  | T_NUMBER _
  | T_BOOL _
  | T_STRING _ ->
    let value = Literal (to_literal current) in
    (match (parent, rest) with
     | (_, []) -> [value], []
     | (Some x, [h]) -> [value], []
     | (None, h :: _) -> failtoken h
     | (Some Array x, h :: n :: r) ->
       (match h.token with
        | T_COMMA ->
          let (v, r2) = walk n parent r in
          value :: v, r2
        | T_RBRACKET -> [value], n :: r
        | _ -> failtoken h)
     | (Some Object x, h :: r) ->
       (match h.token with
        | T_COMMA | T_RCURLY -> [value], h :: r
        | _ -> failtoken h)
     | (Some _, h :: n :: r) ->
       failwith "Unreachable")
  | T_LBRACKET ->
    let (v, rest) = to_array current rest in
    let value = [Array v] in
    (match (parent, rest) with
     | (_, []) -> value, []
     | (None, h :: []) -> failtoken h
     | (Some x, [h]) -> value, []
     | (_, h :: n :: r) ->
       (match h.token with
        | T_COMMA ->
          let (v, r2) = walk n parent r in
          value @ v, r2
        | T_RBRACKET | T_RCURLY -> value, n::r
        | _ -> failtoken h))
  | T_LCURLY ->
    let (v, rest) = to_object current rest in
    let value = [Object v] in
    (match (parent, rest) with
     | (_, []) -> value, []
     | (Some x, [h]) -> value, []
     | (None, h :: _) -> failtoken h
     | (_, h :: n :: r) ->
       (match h.token with
        | T_COMMA ->
          let (v, r2) = walk n parent r in
          value @ v, r2
        | _ -> failtoken h))
  | _ -> failtoken current

let parse (tokens : token_data list): ast_value =
  let (result, r) = match tokens with
    | h :: r -> walk h None r
    | [] -> failwith "unexpected token" in
  List.hd result
