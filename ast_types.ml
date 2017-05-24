
type actual_value =
    Int of int
  | Float of float
  | String of string
  | Bool of bool;;

type ast_literal =
  { _type: string;
    value: actual_value;
    raw_value: string;
    loc: Token.loc };;

type ast_identifier =
  { _type: string;
    value: string;
    loc: Token.loc; };;

type ast_value =
    Literal of ast_literal
  | Array of ast_array
  | Object of ast_object_child

and ast_object_child =
  { _type: string;
    key: ast_identifier;
    value: ast_value;
    loc: Token.loc }

and ast_array =
  { _type: string;
    children: ast_value list;
    loc: Token.loc }

and ast_object =
  { _type: string;
    children: ast_object_child list;
    loc: Token.loc };;