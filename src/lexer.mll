{
  open Lexing
  open Parser5
  open Ast

  let start_pos lexbuf = lexbuf.Lexing.lex_start_p

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
let line_terminator_sequence = '\x0A' | '\x0D' | "\x0D\x0A"
(* TODO LS, PS *)

(* Comment *)
let single_line_comment = "//" not_line_terminator*

(* Identifier *)
(* TODO UnicodeLetter, UnicodeEscapeSequence *)
let identifier_start = ['a'-'z' 'A'-'Z'] | ['$' '_']
let identifier_part  = identifier_start | ['0'-'9']
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
  (* MEMO count lineno of CRLF file *)
  | line_terminator_sequence { new_line lexbuf;
                               token state lexbuf }

  (* Comment *)
  | "/*"                { multi_line_comment state lexbuf }
  | single_line_comment { token state lexbuf }

  (* Identifier + misc *)
  | identifier_name as id
      {
        match id with
        | "break"       -> BREAK (start_pos lexbuf)
        | "do"          -> DO (start_pos lexbuf)
        | "instanceof"  -> INSTANCEOF (start_pos lexbuf)
        | "typeof"      -> TYPEOF (start_pos lexbuf)
        | "case"        -> CASE (start_pos lexbuf)
        | "else"        -> ELSE (start_pos lexbuf)
        | "new"         -> NEW (start_pos lexbuf)
        | "var"         -> VAR (start_pos lexbuf)
        | "catch"       -> CATCH (start_pos lexbuf)
        | "finally"     -> FINALLY (start_pos lexbuf)
        | "return"      -> RETURN (start_pos lexbuf)
        | "void"        -> VOID (start_pos lexbuf)
        | "continue"    -> CONTINUE (start_pos lexbuf)
        | "for"         -> FOR (start_pos lexbuf)
        | "switch"      -> SWITCH (start_pos lexbuf)
        | "while"       -> WHILE (start_pos lexbuf)
        | "debugger"    -> DEBUGGER (start_pos lexbuf)
        | "function"    -> FUNCTION (start_pos lexbuf)
        | "this"        -> THIS (start_pos lexbuf)
        | "with"        -> WITH (start_pos lexbuf)
        | "default"     -> DEFAULT (start_pos lexbuf)
        | "if"          -> IF (start_pos lexbuf)
        | "throw"       -> THROW (start_pos lexbuf)
        | "delete"      -> DELETE (start_pos lexbuf)
        | "in"          -> IN (start_pos lexbuf)
        | "try"         -> TRY (start_pos lexbuf)
        | "class"       -> CLASS (start_pos lexbuf)
        | "enum"        -> ENUM (start_pos lexbuf)
        | "extends"     -> EXTENDS (start_pos lexbuf)
        | "super"       -> SUPER (start_pos lexbuf)
        | "const"       -> CONST (start_pos lexbuf)
        | "export"      -> EXPORT (start_pos lexbuf)
        | "import"      -> IMPORT (start_pos lexbuf)
        | "implements"  -> IMPLEMENTS (start_pos lexbuf)
        | "let"         -> LET (start_pos lexbuf)
        | "private"     -> PRIVATE (start_pos lexbuf)
        | "public"      -> PUBLIC (start_pos lexbuf)
        | "interface"   -> INTERFACE (start_pos lexbuf)
        | "package"     -> PACKAGE (start_pos lexbuf)
        | "protected"   -> PROTECTED (start_pos lexbuf)
        | "static"      -> STATIC (start_pos lexbuf)
        | "yield"       -> YIELD (start_pos lexbuf)
        (* Literal *)
        | "null"        -> NULL (start_pos lexbuf)
        | "true"        -> TRUE (start_pos lexbuf)
        | "false"       -> FALSE (start_pos lexbuf)
        | "get"         -> GET (start_pos lexbuf)
        | "set"         -> SET (start_pos lexbuf)
        | _             -> IDENTIFIER (id, start_pos lexbuf)
      }

  (* Punctuator *)
  | '{'         { LBRACE (start_pos lexbuf) }
  | '}'         { RBRACE (start_pos lexbuf) }
  | '('         { LPAREN (start_pos lexbuf) }
  | ')'         { RPAREN (start_pos lexbuf) }
  | '['         { LBRACK (start_pos lexbuf) }
  | ']'         { RBRACK (start_pos lexbuf) }
  | '.'         { DOT (start_pos lexbuf) }
  | ';'         { SEMI (start_pos lexbuf) }
  | ','         { COMMA (start_pos lexbuf) }
  | "==="       { EQ3 (start_pos lexbuf) }
  | "!=="       { NEQ3 (start_pos lexbuf) }
  | "=="        { EQ2 (start_pos lexbuf) }
  | "!="        { NEQ (start_pos lexbuf) }
  | "++"        { PLUS2 (start_pos lexbuf) }
  | "--"        { MINUS2 (start_pos lexbuf) }
  | "+="        { PLUS_EQ (start_pos lexbuf) }
  | "-="        { MINUS_EQ (start_pos lexbuf) }
  | "*="        { MULT_EQ (start_pos lexbuf) }
  | "/="        { DIV_EQ (start_pos lexbuf) }
  | "%="        { MOD_EQ (start_pos lexbuf) }
  | "<<="       { LT2_EQ (start_pos lexbuf) }
  | ">>="       { GT2_EQ (start_pos lexbuf) }
  | ">>>="      { GT3_EQ (start_pos lexbuf) }
  | "&="        { AND_EQ (start_pos lexbuf) }
  | "|="        { OR_EQ (start_pos lexbuf) }
  | "^="        { XOR_EQ (start_pos lexbuf) }
  | "<="        { LE (start_pos lexbuf) }
  | ">="        { GE (start_pos lexbuf) }
  | "<<"        { LT2 (start_pos lexbuf) }
  | ">>>"       { GT3 (start_pos lexbuf) }
  | ">>"        { GT2 (start_pos lexbuf) }
  | '<'         { LT (start_pos lexbuf) }
  | '>'         { GT (start_pos lexbuf) }
  | '+'         { PLUS (start_pos lexbuf) }
  | '-'         { MINUS (start_pos lexbuf)}
  | '*'         { MULT (start_pos lexbuf) }
  | '/'         { DIV (start_pos lexbuf) }
  | '%'         { MOD (start_pos lexbuf) }
  | "&&"        { AND2 (start_pos lexbuf) }
  | "||"        { OR2 (start_pos lexbuf) }
  | '&'         { AND (start_pos lexbuf) }
  | '|'         { OR (start_pos lexbuf) }
  | '^'         { XOR (start_pos lexbuf) }
  | '!'         { NOT (start_pos lexbuf) }
  | '~'         { NEG (start_pos lexbuf) }
  | '?'         { QUESTION (start_pos lexbuf) }
  | ':'         { COLON (start_pos lexbuf) }
  | '='         { EQ (start_pos lexbuf) }

  (* NumericLiteral *)
  | decimal_literal as dec
      { NUMERIC (`Dec dec, start_pos lexbuf) }
  | hex_integer_literal as hex
      { NUMERIC (`Hex hex, start_pos lexbuf) }

  (* StringLiteral *)
  | '"'         { double_quoted_string state (Buffer.create 16) (start_pos lexbuf) lexbuf }
  | '\''        { single_quoted_string state (Buffer.create 16) (start_pos lexbuf) lexbuf }

  | eof         { EOF }

and multi_line_comment state = parse
  | "*/"        { token state lexbuf }
  | line_terminator_sequence { new_line lexbuf;
                               multi_line_comment state lexbuf }
  | _           { multi_line_comment state lexbuf }

and double_quoted_string state buf pos = parse
  | '"'         { STRING (Buffer.contents buf, pos) }
  | '\\'        { escape_sequence state buf pos double_quoted_string lexbuf }
  (* accept LineTerminator *)
  | _ as c      { Buffer.add_char buf c;
                  double_quoted_string state buf pos lexbuf }

and single_quoted_string state buf pos = parse
  | '\''        { STRING (Buffer.contents buf, pos) }
  | '\\'        { escape_sequence state buf pos single_quoted_string lexbuf }
  (* TODO LineContinuation *)
  (* accept LineTerminator *)
  | _ as c      { Buffer.add_char buf c;
                  single_quoted_string state buf pos lexbuf }

and escape_sequence state buf pos cont = parse
  (* SingleEscapeCharacter *)
  | ['\'' '"' '\\' 'b' 'f' 'n' 'r' 't' 'v'] as c
      { Buffer.add_char buf c;
        cont state buf pos lexbuf }
  (* LineContinuation *)
  | line_terminator_sequence as lts
      { new_line lexbuf;
        Buffer.add_string buf lts;
        cont state buf pos lexbuf }
  (* TODO 0 [lookahead is not DecimalDigit] *)
  (* HexEscapeSequence *)
  | 'x' (hex_digit as h) (hex_digit as l)
      { let c = Char.chr (int_of_hex_char h * 16 + int_of_hex_char l) in
        Buffer.add_char buf c;
        cont state buf pos lexbuf }
  (* TODO UnicodeEscapeSequence *)
  | 'x' (hex_digit as u1) (hex_digit as u2) (hex_digit as u3) (hex_digit as u4)
      { ignore u1; ignore u2; ignore u3; ignore u4;
        failwith "UnicodeEscapeSequence is not supported" }
  (* NonEscapeCharacter *)
  | _ as c
      { Buffer.add_char buf c;
        cont state buf pos lexbuf }
