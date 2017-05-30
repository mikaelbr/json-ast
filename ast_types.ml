
type actual_value =
    Int of int
  | Float of float
  | String of string
  | Bool of bool;;

type ast_literal =
  { _type: Token.t;
    value: actual_value;
    raw_value: actual_value;
    loc: Token.loc };;

type ast_identifier =
  { _type: Token.t;
    value: string;
    loc: Token.loc; };;

type ast_value =
    Literal of ast_literal
  | Array of ast_array
  | Object of ast_object

and ast_object_child =
  { _type: Token.t;
    key: ast_identifier;
    value: ast_value;
    loc: Token.loc }

and ast_array =
  { _type: Token.t;
    children: ast_value list;
    loc: Token.loc }

and ast_object =
  { _type: Token.t;
    children: ast_object_child list;
    loc: Token.loc };;