{
  open Parser5
  open Ast

  let int_of_hex_char c =
    if c >= '0' && c <= '9'
    then Char.code c - Char.code '0'
    else if c >= 'a' && c <= 'f'
    then Char.code c - Char.code 'a'
    else if c >= 'A' && c <= 'F'
    then Char.code c - Char.code 'A'
    else raise (Invalid_argument "invalid hex digit")
}

(* epsilon *)
let e = ""

(* WhiteSpace *)
let white_space = ['\x09' '\x0B' '\x0C' '\x20' '\xA0']
(* TODO BOM, USP *)

(* LineTerminator *)
let line_terminator = ['\x0A' '\x0D']
let not_line_terminator = [^ '\x0A' '\x0D']
let line_terminator_sequece = '\x0A' | '\x0D' | "\x0D\x0A"
(* TODO LS, PS *)

(* Comment *)
let single_line_comment = "//" not_line_terminator*

(* Identifier *)
(* TODO UnicodeLetter, UnicodeEscapeSequence *)
let identifier_start = ['a'-'z' 'A'-'Z' '0'-'9'] | ['$' '_']
let identifier_part  = identifier_start
let identifier_name  = identifier_start+ identifier_part?

(* DecimalLiteral *)
let non_zero_digit = ['1'-'9']
let decimal_digit = ['0'-'9']
let decimal_digits = decimal_digit+
let signed_integer = ['+' '-']? decimal_digits
let exponent_part = ['e' 'E'] signed_integer
let decimal_integer_literal = '0' | non_zero_digit decimal_digits?
let decimal_literal =
  decimal_integer_literal '.' decimal_digits? exponent_part?
  | '.' decimal_digits exponent_part?
  | decimal_integer_literal exponent_part?

(* HexIntegerLiteral *)
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let hex_integer_literal = '0' ['x' 'X'] hex_digit+

(* InputElementDiv *)
rule token state = parse
  (* WhiteSpace *)
  | white_space+        { token state lexbuf }

  (* LineTerminator *)
  | line_terminator+    { token state lexbuf }

  (* Comment *)
  | "/*"                { multi_line_comment state lexbuf }
  | single_line_comment { token state lexbuf }

  (* Identifier + misc *)
  | identifier_name as id
      {
        match id with
        | "break"       -> BREAK
        | "do"          -> DO
        | "instanceof"  -> INSTANCEOF
        | "typeof"      -> TYPEOF
        | "case"        -> CASE
        | "else"        -> ELSE
        | "new"         -> NEW
        | "var"         -> VAR
        | "catch"       -> CATCH
        | "finally"     -> FINALLY
        | "return"      -> RETURN
        | "void"        -> VOID
        | "continue"    -> CONTINUE
        | "for"         -> FOR
        | "switch"      -> SWITCH
        | "while"       -> WHILE
        | "debugger"    -> DEBUGGER
        | "function"    -> FUNCTION
        | "this"        -> THIS
        | "with"        -> WITH
        | "default"     -> DEFAULT
        | "if"          -> IF
        | "throw"       -> THROW
        | "delete"      -> DELETE
        | "in"          -> IN
        | "try"         -> TRY
        | "class"       -> CLASS
        | "enum"        -> ENUM
        | "extends"     -> EXTENDS
        | "super"       -> SUPER
        | "const"       -> CONST
        | "export"      -> EXPORT
        | "import"      -> IMPORT
        | "implements"  -> IMPLEMENTS
        | "let"         -> LET
        | "private"     -> PRIVATE
        | "public"      -> PUBLIC
        | "interface"   -> INTERFACE
        | "package"     -> PACKAGE
        | "protected"   -> PROTECTED
        | "static"      -> STATIC
        | "yield"       -> YIELD
        (* Literal *)
        | "null"        -> NULL
        | "true"        -> TRUE
        | "false"       -> FALSE
        | "get"         -> GET
        | "set"         -> SET
        | _             -> IDENTIFIER id
      }

  (* Punctuator *)
  | '{'         { LBRACE }
  | '}'         { RBRACE }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '['         { LBRACK }
  | ']'         { RBRACK }
  | '.'         { DOT }
  | ';'         { SEMI }
  | ','         { COMMA }
  | "==="       { EQ3 }
  | "!=="       { NEQ3 }
  | "=="        { EQ2 }
  | "!="        { NEQ }
  | "++"        { PLUS2 }
  | "--"        { MINUS2 }
  | "+="        { PLUS_EQ }
  | "-="        { MINUS_EQ }
  | "*="        { MULT_EQ }
  | "/="        { DIV_EQ }
  | "%="        { MOD_EQ }
  | "<<="       { LT2_EQ }
  | ">>="       { GT2_EQ }
  | ">>>="      { GT3_EQ }
  | "&="        { AND_EQ }
  | "|="        { OR_EQ }
  | "^="        { XOR_EQ }
  | "<="        { LE }
  | ">="        { GE }
  | "<<"        { LT2 }
  | ">>>"       { GT3 }
  | ">>"        { GT2 }
  | '<'         { LT }
  | '>'         { GT }
  | '+'         { PLUS }
  | '-'         { MINUS}
  | '*'         { MULT }
  | '/'         { DIV }
  | '%'         { MOD }
  | "&&"        { AND2 }
  | "||"        { OR2 }
  | '&'         { AND }
  | '|'         { OR }
  | '^'         { XOR }
  | '!'         { NOT }
  | '~'         { NEG }
  | '?'         { QUESTION }
  | ':'         { COLON }
  | '='         { EQ }

  (* NumericLiteral *)
  | decimal_literal as dec
      { NUMERIC (`Dec dec) }
  | hex_integer_literal as hex
      { NUMERIC (`Hex hex) }

  (* StringLiteral *)
  | '"'         { double_quoted_string state (Buffer.create 16) lexbuf }
  | '\''        { single_quoted_string state (Buffer.create 16) lexbuf }

  | eof         { EOF }

and multi_line_comment state = parse
  | "*/"        { token state lexbuf }
  | _           { multi_line_comment state lexbuf }

and double_quoted_string state buf = parse
  | '"'         { STRING (Buffer.contents buf) }
  | '\\'        { escape_sequence state buf double_quoted_string lexbuf }
  (* accept LineTerminator *)
  | _ as c      { Buffer.add_char buf c;
                  double_quoted_string state buf lexbuf }

and single_quoted_string state buf = parse
  | '\''        { STRING (Buffer.contents buf) }
  | '\\'        { escape_sequence state buf single_quoted_string lexbuf }
  (* accept LineTerminator *)
  | _ as c      { Buffer.add_char buf c;
                  single_quoted_string state buf lexbuf }

and escape_sequence state buf cont = parse
  (* SingleEscapeCharacter *)
  | ['\'' '"' '\\' 'b' 'f' 'n' 'r' 't' 'v'] as c
      { Buffer.add_char buf c;
        cont state buf lexbuf }
  (* LineContinuation *)
  | line_terminator_sequece as lts
      { Buffer.add_string buf lts;
        cont state buf lexbuf }
  (* TODO 0 [lookahead is not DecimalDigit] *)
  (* HexEscapeSequence *)
  | 'x' (hex_digit as h) (hex_digit as l)
      { let c = Char.chr (int_of_hex_char h * 16 + int_of_hex_char l) in
        Buffer.add_char buf c;
        cont state buf lexbuf }
  (* TODO UnicodeEscapeSequence *)
  | 'x' (hex_digit as u1) (hex_digit as u2) (hex_digit as u3) (hex_digit as u4)
      { ignore u1; ignore u2; ignore u3; ignore u4;
        failwith "UnicodeEscapeSequence is not supported" }
  (* NonEscapeCharacter *)
  | _ as c
      { Buffer.add_char buf c;
        cont state buf lexbuf }
