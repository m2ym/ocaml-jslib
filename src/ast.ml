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
  | Empty of pos
  | Block of stmt list * pos
  | Expr of expr * pos
  | If of expr * stmt * stmt option * pos
  | Labeled of string * stmt * pos
  | Break of string option * pos
  | Continue of string option * pos
  | With of expr * stmt * pos
  | Switch of expr * (expr option * stmt list) list * pos
  | Return of expr option * pos
  | Throw of expr * pos
  | Try of stmt list * (string * stmt list) option * stmt list option * pos
  | While of expr * stmt * pos
  | Do of stmt * expr * pos
  | For of [`Nop | `Var of (string * expr option * pos) list | `Expr of expr] * expr option * expr option * stmt * pos
  | ForIn of [`Var of string * expr option * pos | `Expr of expr] * expr * stmt * pos
  | Debugger of pos
  | FunctionDecl of string * string list * stmt list * pos
  | Var of (string * expr option * pos) list * pos

and expr =
  | Ident of string * pos
  | Literal of literal * pos
  | This of pos
  | Array of expr list * pos
  | Object of property list * pos
  | Function of string option * string list * stmt list * pos
  | Sequence of expr * expr * pos
  | Unary of unary_op * expr * pos
  | Binary of binary_op * expr * expr * pos
  | Assign of assign_op * expr * expr * pos
  | Ternary of expr * expr * expr * pos
  | New of expr * expr list * pos
  | Call of expr * expr list * pos
  | Member of expr * [`Ident of string | `Expr of expr] * pos

and property_name = [`Ident of string | literal]

and property =
  [ `Init of property_name * expr * pos
  | `Get of property_name * stmt list * pos
  | `Set of property_name * string * stmt list * pos ]

let pos_of_expr = function
  | Ident (_,pos) -> pos
  | Literal (_,pos) -> pos
  | This (pos) -> pos
  | Array (_,pos) -> pos
  | Object (_,pos) -> pos
  | Function (_,_,_,pos) -> pos
  | Sequence (_,_,pos) -> pos
  | Unary (_,_,pos) -> pos
  | Binary (_,_,_,pos) -> pos
  | Assign (_,_,_,pos) -> pos
  | Ternary (_,_,_,pos) -> pos
  | New (_,_,pos) -> pos
  | Call (_,_,pos) -> pos
  | Member (_,_,pos) -> pos

let pos_of_stmt = function
  | Empty (pos) -> pos
  | Block (_,pos) -> pos
  | Expr (_,pos) -> pos
  | If (_,_,_,pos) -> pos
  | Labeled (_,_,pos) -> pos
  | Break (_,pos) -> pos
  | Continue (_,pos) -> pos
  | With (_,_,pos) -> pos
  | Switch (_,_,pos) -> pos
  | Return (_,pos) -> pos
  | Throw (_,pos) -> pos
  | Try (_,_,_,pos) -> pos
  | While (_,_,pos) -> pos
  | Do (_,_,pos) -> pos
  | For (_,_,_,_,pos) -> pos
  | ForIn (_,_,_,pos) -> pos
  | Debugger (pos) -> pos
  | FunctionDecl (_,_,_,pos) -> pos
  | Var (_,pos) -> pos
