type pos = Lexing.position

type literal =
  [ `Null
  | `Bool of bool
  | `String of string
  | `Number of [`Dec of string | `Hex of string] ]

type unary_op =
  [ `PostIncr
  | `PostDecr
  | `PreIncr
  | `PreDecr
  | `Not
  | `Negate
  | `Plus
  | `Minus
  | `TypeOf
  | `Void
  | `Delete ]

type binary_op =
  [ `Eq
  | `Neq
  | `Equal
  | `NotEqual
  | `Lt
  | `LtE
  | `Gt
  | `GtE
  | `LShift
  | `AShift
  | `RShift
  | `Add
  | `Sub
  | `Mult
  | `Div
  | `Mod
  | `BitOr
  | `BitAnd
  | `BitXor
  | `In
  | `InstanceOf
  | `Or
  | `And ]

type assign_op =
  [ `Nop
  | `Add
  | `Sub
  | `Mult
  | `Div
  | `Mod
  | `LShift
  | `AShift
  | `RShift
  | `BitOr
  | `BitXor
  | `BitAnd ]

type program = stmt list

and stmt =
  | Empty
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt option
  | Labeled of string * stmt
  | Break of string option
  | Continue of string option
  | With of expr * stmt
  | Switch of expr * (expr option * stmt list) list
  | Return of expr option
  | Throw of expr
  | Try of stmt list * (string * stmt list) option * stmt list option
  | While of expr * stmt
  | Do of stmt * expr
  | For of [`Nop | `Var of (string * expr option) list | `Expr of expr] * expr option * expr option * stmt
  | ForIn of [`Var of string * expr option | `Expr of expr] * expr * stmt
  | Debugger
  | FunctionDecl of string * string list * stmt list
  | Var of (string * expr option) list

and expr =
  | Ident of string
  | Literal of literal
  | This
  | Array of expr list
  | Object of property list
  | Function of string option * string list * stmt list
  | Sequence of expr * expr
  | Unary of unary_op * expr
  | Binary of binary_op * expr * expr
  | Assign of assign_op * expr * expr
  | Ternary of expr * expr * expr
  | New of expr * expr list
  | Call of expr * expr list
  | Member of expr * [`Ident of string | `Expr of expr]

and property_name = [`Ident of string | literal]

and property =
  [ `Init of property_name * expr
  | `Get of property_name * stmt list
  | `Set of property_name * string * stmt list ]
