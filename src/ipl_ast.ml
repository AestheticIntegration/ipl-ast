(** IPL Expression *)

type expr =
  | Paren of expr
  (** Operators *)
  | Not         of expr
  | UnaryMinus  of expr
  | Or          of binary_op 
  | And         of binary_op
  | Mul         of binary_op
  | Add         of binary_op
  | Eq          of binary_op
  | Comp        of binary_op
  | Implies     of binary_op
  | Cont        of binary_op
  | Cond        of { test : expr ; expr : expr ; orelse : expr }
  | Abs         of expr
  | FuncCall    of { name: string; args : expr list}
  (** Option stuff *)
  | IplNone
  | IplSome        of expr
  | OptCase     of { value : expr ; capture : string ; some : expr ; none : expr }
  (** List/Set/Map *)
  | ListLiteral of expr list 
  |  SetLiteral of expr list
  |  MapLiteral of { elements : map_element list ; default : expr }
  | Subset      of { left : expr ; right : expr }
  | Get         of { left : expr ; right : expr }
  (** Constants *)
  |  BoolConst of bool
  |   IntConst of int
  | FloatConst of float
  | StringRef  of string
  (** Inplace record *)
  | Field_vals of field_val list
  (** Converters *)
  | ToInt       of expr
  | ToFloat     of expr 
  | IntOfString of expr
  | Present     of expr
  | ValueRef    of value_ref list
and map_element = { map_key : expr ; map_value : expr }
and binary_op = { left : expr ; op : string ; right : expr }
and field_val = { field : string ; field_value : expr}
and value_ref = {
  name: string;
  index:expr option;
} 

(** IPL Statement *)

type typedecl = 
  | Set    of typedecl
  | Map    of { map_key : typedecl ; map_value : typedecl }
  | List   of typedecl
  | Option of typedecl
  | Simple of string

type field_assignment = {
  name: string;
  value: expr
}

type statement =
  | Assign           of { lvalue : value_ref list ; expr : expr }
  | LetDecl          of { name : string ; ltype : typedecl option ; value : expr }
  | If               of { test : expr ; body : statement list ; orelse : statement list }
  | Case             of { case : expr ; none : statement list ; capture : string ; some : statement list }
  | Insert           of { left : expr ; right : expr ; mapValue : expr option}
  | Remove           of { left : expr ; right : expr }
  | Send             of { message: string; state: string option; _withs: field_assignment list}
  | RangeIteration   of { var : expr; from : expr ; to_ : expr; step : expr; code: statement list}
  | EnumIteration    of { var : expr; enum : typedecl; code: statement list}
  | ReturnStatement  of { ret : expr option}
  
(** IPL Model Statement *)

type field = 
  { name  : string
  ; tag  : string option
  ; ftype : typedecl
  }

type assignable_field_type = 
  | REQ | OPT | AMB

type assignable_field = 
{
  name    : string
  ; ftype   : typedecl
  ; opt_type : assignable_field_type
  ; initial : expr option 
}

type internal_field = 
  { name    : string
  ; ftype   : typedecl
  ; initial : expr option 
  }

type req_type = 
  REQ | OPT | IGN

type require = {
  name: string;
  optionality: req_type;
  validity: expr list
}

type attribute_arg = 
  | Float of float
  | String of string
  | Bool of bool
  | Int of int

type attribute = {
  name: string; 
  args: attribute_arg list
}

type case_decl = {
  name : string;
  tag  : string option
}


type model_statement =
  | Library         of string
  | Import          of string
  | GlobalAttribute  of attribute
  | TypeAlias       of { name : string ; atype  : typedecl }
  | Action          of { name : string ; fields : field list ; validators : expr list }
  | Record          of { name : string ; repeating: bool; fields : field list  } 
  | Enum            of { name : string ; cases  : case_decl list }
  | InternalDecl    of { name : string ; assignable_fields: assignable_field list;internal_fields : internal_field list }
  | Receive         of { event : string ; event_var : string ; body : statement list}
  | Function        of { name : string; args: (string * typedecl) list; returnType:typedecl; body: statement list}
  | Scenario        of { name: string; events : string list}
  | MessageDeclaration of {name: string; tag:string; fields:field list}
  | Message         of {name: string; outbound: bool;requires: require list; validators: expr list}
  | RepeatingGroup  of {name: string; requires: require list}
(** Program *)

type program = model_statement list