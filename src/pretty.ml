open Format
open Ast

let pp_char     = pp_print_char
let pp_string   = pp_print_string
let pp_int      = pp_print_int
let pp_float    = pp_print_float

let pp_opt pp fmt = function
  | None -> ()
  | Some x -> pp fmt x

let rec pp_list ?(sep=", ") pp fmt = function
  | [] -> ()
  | [x] -> pp fmt x
  | x :: xs ->
      fprintf fmt "%a%s%a"
        pp x sep (pp_list pp) xs

let rec pp_seq pp fmt = function
  | [] -> ()
  | [x] -> pp fmt x
  | x :: xs ->
      fprintf fmt "%a@\n" pp x;
      pp_seq pp fmt xs

let rec pp_stmt fmt = function
  | Empty -> pp_string fmt ";"
  | Block body ->
      fprintf fmt "{@\n@[<2>  %a@]@\n}" (pp_seq pp_stmt) body
  | Expr expr ->
      fprintf fmt "%a;" pp_expr expr
  | If (cond, then_, None) ->
      fprintf fmt "if (%a) %a" pp_expr cond pp_stmt then_
  | If (cond, then_, Some else_) ->
      fprintf fmt "if (%a) %a@\nelse %a"
        pp_expr cond
        pp_stmt then_
        pp_stmt else_
  | Labeled (label, body) ->
      fprintf fmt "%s: %a" label pp_stmt body
  | Break (None) ->
      pp_string fmt "break;"
  | Break (Some label) ->
      fprintf fmt "break %s;" label
  | Continue (None) ->
      pp_string fmt "continue;"
  | Continue (Some label) ->
      fprintf fmt "continue %s;" label
  | With (expr, body) ->
      fprintf fmt "with (%a) %a" pp_expr expr pp_stmt body
  | Switch (expr, cases) ->
      fprintf fmt "@[<2>switch (%a) {@\n%a@]@\n}"
        pp_expr expr
        (pp_seq pp_case) cases;
  | Return (None) ->
      pp_string fmt "return;"
  | Return (Some expr) ->
      fprintf fmt "return %a;" pp_expr expr
  | Throw (expr) ->
      fprintf fmt "throw %a;" pp_expr expr
  | Try (body, None, None) ->
      pp_body fmt body
  | Try (body, Some (handler_var, handler_body), None) ->
      fprintf fmt "@[<2>try {@\n%a@]@\n@[<2>} catch (%s) {@\n%a@]@\n}"
        pp_body body
        handler_var
        pp_body handler_body
  | Try (body, None, Some finalizer) ->
      fprintf fmt "@[<2>try {@\n%a@]@\n@[<2>} finally {@\n%a@]@\n}"
        pp_body body
        pp_body finalizer
  | Try (body, Some (handler_var, handler_body), Some finalizer) ->
      fprintf fmt "@[<2>try {@\n%a@]@\n@[<2>} catch (%s) {@\n%a@]@\n@[<2>} finally {@\n%a@]@\n}"
        pp_body body
        handler_var
        pp_body handler_body
        pp_body finalizer
  | While (test, body) ->
      fprintf fmt "while (%a) %a"
        pp_expr test
        pp_stmt body
  | Do (body, test) ->
      fprintf fmt "do %a@\nwhile (%a);"
        pp_stmt body
        pp_expr test
  | For (`Nop, test, update, body) ->
      fprintf fmt "for (; %a; %a) %a"
        (pp_opt pp_expr) test
        (pp_opt pp_expr) update
        pp_stmt body
  | For (`Var decls, test, update, body) ->
      fprintf fmt "for (var %a; %a; %a) %a"
        (pp_list pp_var_decl) decls
        (pp_opt pp_expr) test
        (pp_opt pp_expr) update
        pp_stmt body
  | For (`Expr init, test, update, body) ->
      fprintf fmt "for (%a; %a; %a) %a"
        pp_expr init
        (pp_opt pp_expr) test
        (pp_opt pp_expr) update
        pp_stmt body
  | ForIn (`Var (name, None), generator, body) ->
      fprintf fmt "for (var %s in %a) %a"
        name
        pp_expr generator
        pp_stmt body
  | ForIn (`Var (name, Some init), generator, body) ->
      fprintf fmt "for (var %s = %a in %a) %a"
        name
        pp_expr init
        pp_expr generator
        pp_stmt body
  | ForIn (`Expr lhs, generator, body) ->
      fprintf fmt "for (%a in %a) %a"
        pp_expr lhs
        pp_expr generator
        pp_stmt body
  | Debugger ->
      pp_string fmt "debugger"
  | FunctionDecl (name, params, body) ->
      fprintf fmt "@[<2>function %s(%a) {@\n%a@]@\n}"
        name
        (pp_list pp_string) params
        pp_body body
  | Var decls ->
      fprintf fmt "var %a;" (pp_list pp_var_decl) decls

and pp_case fmt = function
  | (None, body) ->
      fprintf fmt "@[<2>default:@\n%a@]" pp_body body
  | (Some test, body) ->
      fprintf fmt "@[<2>case %a:@\n%a@]" pp_expr test pp_body body

and pp_var_decl fmt (name, init) = match init with
  | None      -> pp_string fmt name
  | Some init -> fprintf fmt "%s = %a" name pp_expr init

and pp_body fmt stmts =
  pp_seq pp_stmt fmt stmts

and pp_expr fmt = function
  | Ident (id) ->
      pp_string fmt id
  | Literal lit ->
      pp_literal fmt lit
  | This ->
      pp_string fmt "this"
  | Array (elements) ->
      fprintf fmt "[%a]" (pp_list pp_expr) elements
  | Object (properties) ->
      fprintf fmt "{%a}" (pp_list pp_property) properties
  | Function (name, params, body) ->
      fprintf fmt "function %a(%a) {%a}"
        (pp_opt pp_string) name
        (pp_list pp_string) params
        pp_body body
  | Sequence (e1, e2) ->
      fprintf fmt "%a, %a" pp_expr e1 pp_expr e2
  | Unary (`PostIncr, expr) ->
      fprintf fmt "%a++" pp_expr expr
  | Unary (`PostDecr, expr) ->
      fprintf fmt "%a--" pp_expr expr
  | Unary (`PreIncr, expr) ->
      fprintf fmt "++%a" pp_expr expr
  | Unary (`PreDecr, expr) ->
      fprintf fmt "--%a" pp_expr expr
  | Unary (`Not, expr) ->
      fprintf fmt "!%a" pp_expr expr
  | Unary (`Negate, expr) ->
      fprintf fmt "~%a" pp_expr expr
  | Unary (`Plus, expr) ->
      fprintf fmt "+%a" pp_expr expr
  | Unary (`Minus, expr) ->
      fprintf fmt "-%a" pp_expr expr
  | Unary (`TypeOf, expr) ->
      fprintf fmt "typeof %a" pp_expr expr
  | Unary (`Void, expr) ->
      fprintf fmt "void %a" pp_expr expr
  | Unary (`Delete, expr) ->
      fprintf fmt "delete %a" pp_expr expr
  | Binary (`Eq, lhs, rhs) ->
      fprintf fmt "(%a == %a)" pp_expr lhs pp_expr rhs
  | Binary (`Neq, lhs, rhs) ->
      fprintf fmt "(%a != %a)" pp_expr lhs pp_expr rhs
  | Binary (`Equal, lhs, rhs) ->
      fprintf fmt "(%a === %a)" pp_expr lhs pp_expr rhs
  | Binary (`NotEqual, lhs, rhs) ->
      fprintf fmt "(%a !== %a)" pp_expr lhs pp_expr rhs
  | Binary (`Lt, lhs, rhs) ->
      fprintf fmt "(%a < %a)" pp_expr lhs pp_expr rhs
  | Binary (`LtE, lhs, rhs) ->
      fprintf fmt "(%a <= %a)" pp_expr lhs pp_expr rhs
  | Binary (`Gt, lhs, rhs) ->
      fprintf fmt "(%a > %a)" pp_expr lhs pp_expr rhs
  | Binary (`GtE, lhs, rhs) ->
      fprintf fmt "(%a >= %a)" pp_expr lhs pp_expr rhs
  | Binary (`LShift, lhs, rhs) ->
      fprintf fmt "(%a << %a)" pp_expr lhs pp_expr rhs
  | Binary (`AShift, lhs, rhs) ->
      fprintf fmt "(%a >> %a)" pp_expr lhs pp_expr rhs
  | Binary (`RShift, lhs, rhs) ->
      fprintf fmt "(%a >>> %a)" pp_expr lhs pp_expr rhs
  | Binary (`Add, lhs, rhs) ->
      fprintf fmt "(%a + %a)" pp_expr lhs pp_expr rhs
  | Binary (`Sub, lhs, rhs) ->
      fprintf fmt "(%a - %a)" pp_expr lhs pp_expr rhs
  | Binary (`Mult, lhs, rhs) ->
      fprintf fmt "(%a * %a)" pp_expr lhs pp_expr rhs
  | Binary (`Div, lhs, rhs) ->
      fprintf fmt "(%a / %a)" pp_expr lhs pp_expr rhs
  | Binary (`Mod, lhs, rhs) ->
      fprintf fmt "(%a %% %a)" pp_expr lhs pp_expr rhs
  | Binary (`BitOr, lhs, rhs) ->
      fprintf fmt "(%a | %a)" pp_expr lhs pp_expr rhs
  | Binary (`BitAnd, lhs, rhs) ->
      fprintf fmt "(%a & %a)" pp_expr lhs pp_expr rhs
  | Binary (`BitXor, lhs, rhs) ->
      fprintf fmt "(%a ^ %a)" pp_expr lhs pp_expr rhs
  | Binary (`In, lhs, rhs) ->
      fprintf fmt "(%a in %a)" pp_expr lhs pp_expr rhs
  | Binary (`InstanceOf, lhs, rhs) ->
      fprintf fmt "(%a instanceof %a)" pp_expr lhs pp_expr rhs
  | Binary (`Or, lhs, rhs) ->
      fprintf fmt "(%a || %a)" pp_expr lhs pp_expr rhs
  | Binary (`And, lhs, rhs) ->
      fprintf fmt "(%a && %a)" pp_expr lhs pp_expr rhs
  | Assign (`Nop, lhs, rhs) ->
      fprintf fmt "%a = %a" pp_expr lhs pp_expr rhs
  | Assign (`Add, lhs, rhs) ->
      fprintf fmt "%a += %a" pp_expr lhs pp_expr rhs
  | Assign (`Sub, lhs, rhs) ->
      fprintf fmt "%a -= %a" pp_expr lhs pp_expr rhs
  | Assign (`Mult, lhs, rhs) ->
      fprintf fmt "%a *= %a" pp_expr lhs pp_expr rhs
  | Assign (`Div, lhs, rhs) ->
      fprintf fmt "%a /= %a" pp_expr lhs pp_expr rhs
  | Assign (`Mod, lhs, rhs) ->
      fprintf fmt "%a %%= %a" pp_expr lhs pp_expr rhs
  | Assign (`LShift, lhs, rhs) ->
      fprintf fmt "%a <<= %a" pp_expr lhs pp_expr rhs
  | Assign (`AShift, lhs, rhs) ->
      fprintf fmt "%a >>= %a" pp_expr lhs pp_expr rhs
  | Assign (`RShift, lhs, rhs) ->
      fprintf fmt "%a >>>= %a" pp_expr lhs pp_expr rhs
  | Assign (`BitOr, lhs, rhs) ->
      fprintf fmt "%a |= %a" pp_expr lhs pp_expr rhs
  | Assign (`BitXor, lhs, rhs) ->
      fprintf fmt "%a ^= %a" pp_expr lhs pp_expr rhs
  | Assign (`BitAnd, lhs, rhs) ->
      fprintf fmt "%a &= %a" pp_expr lhs pp_expr rhs
  | Ternary (cond, then_, else_) ->
      fprintf fmt "(%a ? %a : %a)" pp_expr cond pp_expr then_ pp_expr else_
  | New (expr, args) ->
      fprintf fmt "new %a(%a)" pp_expr expr (pp_list pp_expr) args
  | Call (expr, args) ->
      fprintf fmt "%a(%a)" pp_expr expr (pp_list pp_expr) args
  | Member (expr, `Ident id) ->
      fprintf fmt "%a.%s" pp_expr expr id
  | Member (expr, `Expr e) ->
      fprintf fmt "%a[%a]" pp_expr expr pp_expr e

and pp_literal fmt = function
  | `Null ->
      pp_string fmt "null"
  | `Bool true ->
      pp_string fmt "true"
  | `Bool false ->
      pp_string fmt "false"
  | `String str ->
      pp_double_quoted_string fmt str
  | `Number (`Dec dec) ->
      pp_string fmt dec
  | `Number (`Hex hex) ->
      fprintf fmt "0x%s" hex

and pp_double_quoted_string fmt s =
  (* TODO escape *)
  fprintf fmt "\"%s\"" s

and pp_property fmt = function
  | `Init (name, init) ->
      fprintf fmt "%a : %a" pp_property_name name pp_expr init
  | `Get (name, body) ->
      fprintf fmt "get %a() {%a}" pp_property_name name pp_body body
  | `Set (name, param, body) ->
      fprintf fmt "set %a(%s) {%a}" pp_property_name name param pp_body body

and pp_property_name fmt = function
  | `Ident id       -> pp_string fmt id
  | #literal as lit -> pp_literal fmt lit

let pp_program fmt program = pp_body fmt program

let pp_print_program fmt program = pp_program fmt program
let print_program program = pp_print_program std_formatter program
