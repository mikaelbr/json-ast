open Token
open Ast_types
open Batteries

let to_actual_value = function
  | T_NUMBER raw -> String raw, Int (int_of_string raw)
  | T_STRING raw -> String raw, String raw
  | T_BOOL raw -> Bool raw, Bool raw
  | _ -> failwith "unexpected literal";;

let to_literal current =
  let (raw, value) = to_actual_value current.token in
  { _type = current.token;
    raw_value = raw;
    value = value;
    loc = current.loc };;

let prop key_string key_loc value : ast_object_child =
  { _type = T_PROPERTY;
    key = { _type = T_IDENTIFIER;
            value = key_string;
            loc = key_loc };
    value = value;
    loc = key_loc };;

let failtoken token_data =
  failwith ("unexpected token " ^ string_of_token token_data.token);;

let rec to_array (current : token_data) rest : ast_array * 'a =
  let parent : ast_array = { _type = T_ARRAY;
                             loc = current.loc;
                             children = []} in
  let (children, r) = match rest with
    | h :: r ->
      (match h.token with
       | T_RBRACKET -> [], r
       | _ -> walk (Some (Array parent)) (h :: r))
    | _ -> failwith "unreachable" in
  { parent with children = children }, r

and to_property parent = function
  | { token = T_STRING x; loc } :: { token = T_COLON } :: r ->
    let (c, r) = walk parent r in
    prop x loc (List.hd c), r
  | { token = T_STRING _ } :: r :: _
  | r :: _ -> failtoken r
  | _ -> failwith "Unexpected end"

and property_children parent_object rest =
  let (child, rest) = to_property parent_object rest in
  match rest with
  | [] -> [child], []
  | h :: r ->
    (match h.token with
     | T_RCURLY -> [child], r
     | T_COMMA ->
       let (c, rest) = property_children parent_object r in
       child :: c, rest
     | _ -> failwith "unreachable")

and to_object (current: token_data) rest =
  let get_children parent = function
    | { token = T_RCURLY } :: r -> [], r
    | _ -> property_children parent rest in
  let parent : ast_object = { _type = T_OBJECT;
                              loc = current.loc;
                              children = [] } in
  let parent_object = Some (Object parent) in
  let (children, rest) = get_children parent_object rest in
  { parent with children }, rest

and walk parent = function
  | current :: rest -> (
      match current.token with
      | T_NUMBER _
      | T_BOOL _
      | T_STRING _ ->
        let value = Literal (to_literal current) in
        (match (parent, rest) with
         | (_, []) | (Some _, [_]) -> [value], []
         | (None, h :: _) -> failtoken h
         | (Some Array _, { token = T_RBRACKET } :: r)
         | (Some Object _, { token = T_COMMA } :: r)
         | (Some Object _, { token = T_RCURLY } :: r) -> [value], r
         | (Some Array x, { token = T_COMMA} :: r) ->
           let (v, r) = walk parent r in
           value :: v, r
         | (Some _, h :: n :: r) -> failtoken h)
      | T_LBRACKET ->
        let (v, rest) = to_array current rest in
        let value = [Array v] in
        (match (parent, rest) with
         | (_, []) | (Some _, [_]) -> value, []
         | (None, h :: []) -> failtoken h
         | (_, { token = T_COMMA } :: n :: r) ->
           let (v, r2) = walk parent (n :: r) in
           value @ v, r2
         | (_, { token = T_RBRACKET } :: r)
         | (_, { token = T_RCURLY } :: r) -> value, r
         | (_, h :: _) -> failtoken h)
      | T_LCURLY ->
        let (v, rest) = to_object current rest in
        let value = [Object v] in
        (match (parent, rest) with
         | (_, []) -> value, []
         | (Some x, [h]) -> value, []
         | (None, h :: _) -> failtoken h
         | (_, { token = T_COMMA } :: n :: r) ->
           let (v, r2) = walk parent (n :: r) in
           value @ v, r2
         | (_, h::_) -> failtoken h)
      | _ -> failtoken current)
  | _ -> failwith "unexpected end";;

let parse =
  List.hd << fst << walk None;;