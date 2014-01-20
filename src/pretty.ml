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
  | Empty _ -> pp_string fmt ";"
  | Block (body, _) ->
      fprintf fmt "{@\n@[<2>  %a@]@\n}" (pp_seq pp_stmt) body
  | Expr (expr, _) ->
      fprintf fmt "%a;" pp_expr expr
  | If (cond, then_, None, _) ->
      fprintf fmt "if (%a) %a" pp_expr cond pp_stmt then_
  | If (cond, then_, Some else_, _) ->
      fprintf fmt "if (%a) %a@\nelse %a"
        pp_expr cond
        pp_stmt then_
        pp_stmt else_
  | Labeled (label, body, _) ->
      fprintf fmt "%s: %a" label pp_stmt body
  | Break (None, _) ->
      pp_string fmt "break;"
  | Break (Some label, _) ->
      fprintf fmt "break %s;" label
  | Continue (None, _) ->
      pp_string fmt "continue;"
  | Continue (Some label, _) ->
      fprintf fmt "continue %s;" label
  | With (expr, body, _) ->
      fprintf fmt "with (%a) %a" pp_expr expr pp_stmt body
  | Switch (expr, cases, _) ->
      fprintf fmt "@[<2>switch (%a) {@\n%a@]@\n}"
        pp_expr expr
        (pp_seq pp_case) cases;
  | Return (None, _) ->
      pp_string fmt "return;"
  | Return (Some expr, _) ->
      fprintf fmt "return %a;" pp_expr expr
  | Throw (expr, _) ->
      fprintf fmt "throw %a;" pp_expr expr
  | Try (body, None, None, _) ->
      pp_body fmt body
  | Try (body, Some (handler_var, handler_body), None, _) ->
      fprintf fmt "@[<2>try {@\n%a@]@\n@[<2>} catch (%s) {@\n%a@]@\n}"
        pp_body body
        handler_var
        pp_body handler_body
  | Try (body, None, Some finalizer, _) ->
      fprintf fmt "@[<2>try {@\n%a@]@\n@[<2>} finally {@\n%a@]@\n}"
        pp_body body
        pp_body finalizer
  | Try (body, Some (handler_var, handler_body), Some finalizer, _) ->
      fprintf fmt "@[<2>try {@\n%a@]@\n@[<2>} catch (%s) {@\n%a@]@\n@[<2>} finally {@\n%a@]@\n}"
        pp_body body
        handler_var
        pp_body handler_body
        pp_body finalizer
  | While (test, body, _) ->
      fprintf fmt "while (%a) %a"
        pp_expr test
        pp_stmt body
  | Do (body, test, _) ->
      fprintf fmt "do %a@\nwhile (%a);"
        pp_stmt body
        pp_expr test
  | For (`Nop, test, update, body, _) ->
      fprintf fmt "for (; %a; %a) %a"
        (pp_opt pp_expr) test
        (pp_opt pp_expr) update
        pp_stmt body
  | For (`Var decls, test, update, body, _) ->
      fprintf fmt "for (var %a; %a; %a) %a"
        (pp_list pp_var_decl) decls
        (pp_opt pp_expr) test
        (pp_opt pp_expr) update
        pp_stmt body
  | For (`Expr init, test, update, body, _) ->
      fprintf fmt "for (%a; %a; %a) %a"
        pp_expr init
        (pp_opt pp_expr) test
        (pp_opt pp_expr) update
        pp_stmt body
  | ForIn (`Var (name, None, _), generator, body, _) ->
      fprintf fmt "for (var %s in %a) %a"
        name
        pp_expr generator
        pp_stmt body
  | ForIn (`Var (name, Some init, _), generator, body, _) ->
      fprintf fmt "for (var %s = %a in %a) %a"
        name
        pp_expr init
        pp_expr generator
        pp_stmt body
  | ForIn (`Expr lhs, generator, body, _) ->
      fprintf fmt "for (%a in %a) %a"
        pp_expr lhs
        pp_expr generator
        pp_stmt body
  | Debugger _ ->
      pp_string fmt "debugger"
  | FunctionDecl (name, params, body, _) ->
      fprintf fmt "@[<2>function %s(%a) {@\n%a@]@\n}"
        name
        (pp_list pp_string) params
        pp_body body
  | Var (decls, _) ->
      fprintf fmt "var %a;" (pp_list pp_var_decl) decls

and pp_case fmt = function
  | (None, body) ->
      fprintf fmt "@[<2>default:@\n%a@]" pp_body body
  | (Some test, body) ->
      fprintf fmt "@[<2>case %a:@\n%a@]" pp_expr test pp_body body

and pp_var_decl fmt (name, init, _) = match init with
  | None      -> pp_string fmt name
  | Some init -> fprintf fmt "%s = %a" name pp_expr init

and pp_body fmt stmts =
  pp_seq pp_stmt fmt stmts

and pp_expr fmt = function
  | Ident (id, _) ->
      pp_string fmt id
  | Literal (lit, _) ->
      pp_literal fmt lit
  | This _ ->
      pp_string fmt "this"
  | Array (elements, _) ->
      fprintf fmt "[%a]" (pp_list pp_expr) elements
  | Object (properties, _) ->
      fprintf fmt "{%a}" (pp_list pp_property) properties
  | Function (name, params, body, _) ->
      fprintf fmt "function %a(%a) {%a}"
        (pp_opt pp_string) name
        (pp_list pp_string) params
        pp_body body
  | Sequence (e1, e2, _) ->
      fprintf fmt "%a, %a" pp_expr e1 pp_expr e2
  | Unary (`PostIncr, expr, _) ->
      fprintf fmt "%a++" pp_expr expr
  | Unary (`PostDecr, expr, _) ->
      fprintf fmt "%a--" pp_expr expr
  | Unary (`PreIncr, expr, _) ->
      fprintf fmt "++%a" pp_expr expr
  | Unary (`PreDecr, expr, _) ->
      fprintf fmt "--%a" pp_expr expr
  | Unary (`Not, expr, _) ->
      fprintf fmt "!%a" pp_expr expr
  | Unary (`Negate, expr, _) ->
      fprintf fmt "~%a" pp_expr expr
  | Unary (`Plus, expr, _) ->
      fprintf fmt "+%a" pp_expr expr
  | Unary (`Minus, expr, _) ->
      fprintf fmt "-%a" pp_expr expr
  | Unary (`TypeOf, expr, _) ->
      fprintf fmt "typeof %a" pp_expr expr
  | Unary (`Void, expr, _) ->
      fprintf fmt "void %a" pp_expr expr
  | Unary (`Delete, expr, _) ->
      fprintf fmt "delete %a" pp_expr expr
  | Binary (op, lhs, rhs, _) ->
      fprintf fmt "(%a %s %a)"
        pp_expr lhs
        (string_of_binary_op op)
        pp_expr rhs
  | Assign (op, lhs, rhs, _) ->
      fprintf fmt "%a %s= %a"
        pp_expr lhs
        (string_of_assign_op op)
        pp_expr rhs
  | Ternary (cond, then_, else_, _) ->
      fprintf fmt "(%a ? %a : %a)" pp_expr cond pp_expr then_ pp_expr else_
  | New (expr, args, _) ->
      fprintf fmt "new %a(%a)" pp_expr expr (pp_list pp_expr) args
  | Call (expr, args, _) ->
      fprintf fmt "%a(%a)" pp_expr expr (pp_list pp_expr) args
  | Member (expr, `Ident id, _) ->
      fprintf fmt "%a.%s" pp_expr expr id
  | Member (expr, `Expr e, _) ->
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
  | `Init (name, init, _) ->
      fprintf fmt "%a : %a" pp_property_name name pp_expr init
  | `Get (name, body, _) ->
      fprintf fmt "get %a() {%a}" pp_property_name name pp_body body
  | `Set (name, param, body, _) ->
      fprintf fmt "set %a(%s) {%a}" pp_property_name name param pp_body body

and pp_property_name fmt = function
  | `Ident id       -> pp_string fmt id
  | #literal as lit -> pp_literal fmt lit

let pp_program fmt program = pp_body fmt program

let pp_print_program fmt program = pp_program fmt program
let print_program program = pp_print_program std_formatter program
