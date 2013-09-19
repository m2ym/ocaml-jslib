%{
  open Ast
%}

/* Identifier */
%token <string * Lexing.position>
    IDENTIFIER
%token <Lexing.position>
    GET SET
/* ReservedWord */
%token <Lexing.position>
    BREAK DO INSTANCEOF TYPEOF
    CASE ELSE NEW VAR
    CATCH FINALLY RETURN VOID
    CONTINUE FOR SWITCH WHILE
    DEBUGGER FUNCTION THIS WITH
    DEFAULT IF THROW
    DELETE IN TRY
/* FutureReservedWord */
%token <Lexing.position>
    CLASS ENUM EXTENDS SUPER
    CONST EXPORT IMPORT
    IMPLEMENTS LET PRIVATE PUBLIC
    INTERFACE PACKAGE PROTECTED STATIC
    YIELD
/* Punctuator */
%token <Lexing.position>
    LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
    DOT SEMI COMMA LT GT LE GE
    EQ2 NEQ EQ3 NEQ3
    PLUS MINUS MULT DIV MOD PLUS2 MINUS2
    LT2 GT2 GT3 AND OR XOR
    NOT NEG AND2 OR2 QUESTION COLON
    EQ PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ MOD_EQ LT2_EQ
    GT2_EQ GT3_EQ AND_EQ OR_EQ XOR_EQ
%token <Lexing.position>
    NULL TRUE FALSE
%token <[`Dec of string | `Hex of string] * Lexing.position>
    NUMERIC
%token <string * Lexing.position>
    STRING
%token
    EOF

%start statement
%start program
%type <Ast.stmt> statement
%type <Ast.program> program

%%

auto_semi: SEMI {} | {}

identifier:
    IDENTIFIER  { $1 }
  | GET         { "get", $1 }
  | SET         { "set", $1 }

identifier_name:
    identifier  { $1 }
  | BREAK       { "break", $1 }
  | DO          { "do", $1 }
  | INSTANCEOF  { "instanceof", $1 }
  | TYPEOF      { "typeof", $1 }
  | CASE        { "case", $1 }
  | ELSE        { "else", $1 }
  | NEW         { "new", $1 }
  | VAR         { "var", $1 }
  | CATCH       { "catch", $1 }
  | FINALLY     { "finally", $1 }
  | RETURN      { "return", $1 }
  | VOID        { "void", $1 }
  | CONTINUE    { "continue", $1 }
  | FOR         { "for", $1 }
  | SWITCH      { "switch", $1 }
  | WHILE       { "while", $1 }
  | DEBUGGER    { "debugger", $1 }
  | FUNCTION    { "function", $1 }
  | THIS        { "this", $1 }
  | WITH        { "with", $1 }
  | DEFAULT     { "default", $1 }
  | IF          { "if", $1 }
  | THROW       { "throw", $1 }
  | DELETE      { "delete", $1 }
  | IN          { "in", $1 }
  | TRY         { "try", $1 }
  | CLASS       { "class", $1 }
  | ENUM        { "enum", $1 }
  | EXTENDS     { "extends", $1 }
  | SUPER       { "super", $1 }
  | CONST       { "const", $1 }
  | EXPORT      { "export", $1 }
  | IMPORT      { "import", $1 }
  | IMPLEMENTS  { "implements", $1 }
  | LET         { "let", $1 }
  | PRIVATE     { "private", $1 }
  | PUBLIC      { "public", $1 }
  | INTERFACE   { "interface", $1 }
  | PACKAGE     { "package", $1 }
  | PROTECTED   { "protected", $1 }
  | STATIC      { "static", $1 }
  | YIELD       { "yield", $1 }
  | NULL        { "null", $1 }
  | TRUE        { "true", $1 }
  | FALSE       { "false", $1 }

literal:
    NULL        { `Null, $1 }
  | TRUE        { `Bool true, $1 }
  | FALSE       { `Bool false, $1 }
  | NUMERIC     { `Number (fst $1), snd $1 }
  | STRING      { `String (fst $1), snd $1 }
/* TODO RegularExpressionLiteral */

primary_expression:
    THIS                        { This $1 }
  | identifier                  { Ident (fst $1, snd $1) }
  | literal                     { Literal (fst $1, snd $1) }
  | array_literal               { $1 }
  | object_literal              { $1 }
  | LPAREN expression RPAREN    { $2 }

array_literal:
    LBRACK RBRACK
      { Array ([], $1) }
  | LBRACK element_list elision_opt RBRACK
      { Array ($2, $1) }

element_list:
    elision_opt assignment_expression
      { [$2] }
  | element_list COMMA elision_opt assignment_expression
      { $1 @ [$4] }

elision: COMMA {} | elision COMMA {}

elision_opt: elision {} | {}

object_literal:
    LBRACE RBRACE
      { Object ([], $1) }
  | LBRACE property_name_and_value_list RBRACE
      { Object ($2, $1) }
  | LBRACE property_name_and_value_list COMMA RBRACE
      { Object ($2, $1) }

property_name_and_value_list:
    property_assignment
      { [$1] }
  | property_name_and_value_list COMMA property_assignment
      { $1 @ [$3] }

property_assignment:
    property_name COLON assignment_expression
      { `Init (fst $1, $3, snd $1) }
  | GET property_name LPAREN RPAREN block
      { `Get (fst $2, fst $5, snd $2) }
  | SET property_name LPAREN identifier RPAREN block
      { `Set (fst $2, (fst $4), fst $6, snd $2) }

property_name:
    identifier          { `Ident (fst $1), snd $1 }
  | STRING              { `String (fst $1), snd $1 }
  | NUMERIC             { `Number (fst $1), snd $1 }

member_expression:
    primary_expression                          { $1 }
  | function_expression                         { $1 }
  | member_expression LBRACK expression RBRACK  { Member ($1, `Expr $3, $2) }
  | member_expression DOT identifier_name       { Member ($1, `Ident (fst $3), $2) }
  | NEW member_expression arguments             { New ($2, $3, $1) }

new_expression:
    member_expression                           { $1 }
  | NEW new_expression                          { New ($2, [], $1) }

call_expression:
    member_expression arguments                 { Call ($1, $2, pos_of_expr $1) }
  | call_expression arguments                   { Call ($1, $2, pos_of_expr $1) }
  | call_expression LBRACK expression RBRACK    { Member ($1, `Expr $3, $2) }
  | call_expression DOT identifier_name         { Member ($1, `Ident (fst $3), $2) }

arguments:
    LPAREN RPAREN               { [] }
  | LPAREN argument_list RPAREN { $2 }

argument_list:
    assignment_expression                       { [$1] }
  | argument_list COMMA assignment_expression   { $1 @ [$3] }

left_hand_side_expression:
    new_expression      { $1 }
  | call_expression     { $1 }

postfix_expression:
    left_hand_side_expression           { $1 }
  | left_hand_side_expression PLUS2     { Unary (`PostIncr, $1, $2) }
  | left_hand_side_expression MINUS2    { Unary (`PostDecr, $1, $2) }

unary_expression:
    postfix_expression                  { $1 }
  | DELETE unary_expression             { Unary (`Delete, $2, $1) }
  | VOID unary_expression               { Unary (`Void, $2, $1) }
  | TYPEOF unary_expression             { Unary (`TypeOf, $2, $1) }
  | PLUS2 unary_expression              { Unary (`PreIncr, $2, $1) }
  | MINUS2 unary_expression             { Unary (`PreDecr, $2, $1) }
  | PLUS unary_expression               { Unary (`Plus, $2, $1) }
  | MINUS unary_expression              { Unary (`Minus, $2, $1) }
  | NEG unary_expression                { Unary (`Negate, $2, $1) }
  | NOT unary_expression                { Unary (`Not, $2, $1) }

multiplicative_expression:
    unary_expression
      { $1 }
  | multiplicative_expression MULT unary_expression
      { Binary (`Mult, $1, $3, $2) }
  | multiplicative_expression DIV unary_expression
      { Binary (`Div, $1, $3, $2) }
  | multiplicative_expression MOD unary_expression
      { Binary (`Mod, $1, $3, $2) }

additive_expression:
    multiplicative_expression
      { $1 }
  | additive_expression PLUS multiplicative_expression
      { Binary (`Add, $1, $3, $2) }
  | additive_expression MINUS multiplicative_expression
      { Binary (`Sub, $1, $3, $2) }

shift_expression:
    additive_expression
      { $1 }
  | shift_expression LT2 additive_expression
      { Binary (`LShift, $1, $3, $2) }
  | shift_expression GT2 additive_expression
      { Binary (`AShift, $1, $3, $2) }
  | shift_expression GT3 additive_expression
      { Binary (`RShift, $1, $3, $2) }

relational_expression:
    shift_expression
      { $1 }
  | relational_expression LT shift_expression
      { Binary (`Lt, $1, $3, $2) }
  | relational_expression GT shift_expression
      { Binary (`Gt, $1, $3, $2) }
  | relational_expression LE shift_expression
      { Binary (`LtE, $1, $3, $2) }
  | relational_expression GE shift_expression
      { Binary (`GtE, $1, $3, $2) }
  | relational_expression INSTANCEOF shift_expression
      { Binary (`InstanceOf, $1, $3, $2) }
  | relational_expression IN shift_expression
      { Binary (`In, $1, $3, $2) }

relational_expression_no_in:
    shift_expression
      { $1 }
  | relational_expression_no_in LT shift_expression
      { Binary (`Lt, $1, $3, $2) }
  | relational_expression_no_in GT shift_expression
      { Binary (`Gt, $1, $3, $2) }
  | relational_expression_no_in LE shift_expression
      { Binary (`LtE, $1, $3, $2) }
  | relational_expression_no_in GE shift_expression
      { Binary (`GtE, $1, $3, $2) }
  | relational_expression_no_in INSTANCEOF shift_expression
      { Binary (`InstanceOf, $1, $3, $2) }

equality_expression:
    relational_expression
      { $1 }
  | equality_expression EQ2 relational_expression
      { Binary (`Eq, $1, $3, $2) }
  | equality_expression NEQ relational_expression
      { Binary (`Neq, $1, $3, $2) }
  | equality_expression EQ3 relational_expression
      { Binary (`Equal, $1, $3, $2) }
  | equality_expression NEQ3 relational_expression
      { Binary (`NotEqual, $1, $3, $2) }

equality_expression_no_in:
    relational_expression_no_in
      { $1 }
  | equality_expression_no_in EQ2 relational_expression_no_in
      { Binary (`Eq, $1, $3, $2) }
  | equality_expression_no_in NEQ relational_expression_no_in
      { Binary (`Neq, $1, $3, $2) }
  | equality_expression_no_in EQ3 relational_expression_no_in
      { Binary (`Equal, $1, $3, $2) }
  | equality_expression_no_in NEQ3 relational_expression_no_in
      { Binary (`NotEqual, $1, $3, $2) }

bitwise_AND_expression:
    equality_expression
      { $1 }
  | bitwise_AND_expression AND equality_expression
      { Binary (`BitAnd, $1, $3, $2) }

bitwise_AND_expression_no_in:
    equality_expression_no_in
      { $1 }
  | bitwise_AND_expression_no_in AND equality_expression_no_in
      { Binary (`BitAnd, $1, $3, $2) }

bitwise_XOR_expression:
    bitwise_AND_expression
      { $1 }
  | bitwise_XOR_expression XOR bitwise_AND_expression
      { Binary (`BitXor, $1, $3, $2) }

bitwise_XOR_expression_no_in:
    bitwise_AND_expression_no_in
      { $1 }
  | bitwise_XOR_expression_no_in XOR bitwise_AND_expression_no_in
      { Binary (`BitXor, $1, $3, $2) }

bitwise_OR_expression:
    bitwise_XOR_expression
      { $1 }
  | bitwise_OR_expression OR bitwise_XOR_expression
      { Binary (`BitOr, $1, $3, $2) }

bitwise_OR_expression_no_in:
    bitwise_XOR_expression_no_in
      { $1 }
  | bitwise_OR_expression_no_in OR bitwise_XOR_expression_no_in
      { Binary (`BitOr, $1, $3, $2) }

logical_AND_expression:
    bitwise_OR_expression
      { $1 }
  | logical_AND_expression AND2 bitwise_OR_expression
      { Binary (`And, $1, $3, $2) }

logical_AND_expression_no_in:
    bitwise_OR_expression_no_in
      { $1 }
  | logical_AND_expression_no_in AND2 bitwise_OR_expression_no_in
      { Binary (`And, $1, $3, $2) }

logical_OR_expression:
    logical_AND_expression
      { $1 }
  | logical_OR_expression OR2 logical_AND_expression
      { Binary (`Or, $1, $3, $2) }

logical_OR_expression_no_in:
    logical_AND_expression_no_in
      { $1 }
  | logical_OR_expression_no_in OR2 logical_AND_expression_no_in
      { Binary (`Or, $1, $3, $2) }

conditional_expression:
    logical_OR_expression
      { $1 }
  | logical_OR_expression QUESTION assignment_expression COLON assignment_expression
      { Ternary ($1, $3, $5, $2) }

conditional_expression_no_in:
    logical_OR_expression_no_in
      { $1 }
  | logical_OR_expression_no_in QUESTION assignment_expression COLON assignment_expression_no_in
      { Ternary ($1, $3, $5, $2) }

assignment_expression:
    conditional_expression
      { $1 }
  | left_hand_side_expression EQ assignment_expression
      { Assign (`Nop, $1, $3, $2) }
  | left_hand_side_expression assignment_operator assignment_expression
      { Assign (fst $2, $1, $3, snd $2) }

 assignment_expression_no_in:
    conditional_expression_no_in
      { $1 }
  | left_hand_side_expression EQ assignment_expression_no_in
      { Assign (`Nop, $1, $3, $2) }
  | left_hand_side_expression assignment_operator assignment_expression_no_in
      { Assign (fst $2, $1, $3, snd $2) }

assignment_operator:
    MULT_EQ     { `Mult, $1 }
  | DIV_EQ      { `Div, $1 }
  | MOD_EQ      { `Mod, $1 }
  | PLUS_EQ     { `Add, $1 }
  | MINUS_EQ    { `Sub, $1 }
  | LT2_EQ      { `LShift, $1 }
  | GT2_EQ      { `AShift, $1 }
  | GT3_EQ      { `RShift, $1 }
  | AND_EQ      { `BitAnd, $1 }
  | XOR_EQ      { `BitXor, $1 }
  | OR_EQ       { `BitOr, $1 }

expression:
    assignment_expression
      { $1 }
  | expression COMMA assignment_expression
      { Sequence ($1, $3, $2) }

expression_opt: { None } | expression { Some $1 }

expression_no_in:
    assignment_expression_no_in
      { $1 }
  | expression_no_in COMMA assignment_expression_no_in
      { Sequence ($1, $3, $2) }

statement:
    block                       { Block (fst $1, snd $1) }
  | function_declaration        { $1 }
  | variable_statement          { $1 }
  | empty_statement             { $1 }
  | expression_statement        { $1 }
  | if_statement                { $1 }
  | iteration_statement         { $1 }
  | continue_statement          { $1 }
  | break_statement             { $1 }
  | return_statement            { $1 }
  | with_statement              { $1 }
  | labeled_statement           { $1 }
  | switch_statement            { $1 }
  | throw_statement             { $1 }
  | try_statement               { $1 }
  | debugger_statement          { $1 }

block:
    LBRACE RBRACE                       { [], $1 }
  | LBRACE statement_list RBRACE        { $2, $1 }

statement_list:
    statement                   { [$1] }
  | statement_list statement    { $1 @ [$2] }

statement_list_opt: statement_list { $1 } |  { [] }

variable_statement:
    VAR variable_declaration_list auto_semi
      { Var ($2, $1) }

variable_declaration_list:
    variable_declaration
      { [$1] }
  | variable_declaration_list COMMA variable_declaration
      { $1 @ [$3] }

variable_declaration_list_no_in:
    variable_declaration_no_in
      { [$1] }
  | variable_declaration_list_no_in COMMA variable_declaration_no_in
      { $1 @ [$3] }

variable_declaration:
    identifier initializer_opt  { (fst $1, $2, snd $1) }

variable_declaration_no_in:
    identifier initializer_no_in_opt  { (fst $1, $2, snd $1) }

initializer_:
    EQ assignment_expression    { $2 }

initializer_opt: initializer_ { Some $1 } |  { None }

initializer_no_in:
    EQ assignment_expression_no_in      { $2 }

initializer_no_in_opt: initializer_no_in { Some $1 } | { None }

empty_statement:
    SEMI { Empty ($1) }

expression_statement:
    expression auto_semi { Expr ($1, pos_of_expr $1) }

if_statement:
    IF LPAREN expression RPAREN statement ELSE statement
      { If ($3, $5, Some $7, $1) }
  | IF LPAREN expression RPAREN statement
      { If ($3, $5, None, $1) }

iteration_statement:
    DO statement WHILE LPAREN expression RPAREN auto_semi
      { Do ($2, $5, $1) }
  | WHILE LPAREN expression RPAREN statement
      { While ($3, $5, $1) }
  | FOR LPAREN SEMI expression_opt SEMI expression_opt RPAREN statement
      { For (`Nop, $4, $6, $8, $1) }
  | FOR LPAREN expression_no_in SEMI expression_opt SEMI expression_opt RPAREN statement
      { For (`Expr $3, $5, $7, $9, $1) }
  | FOR LPAREN VAR variable_declaration_list_no_in SEMI expression_opt SEMI expression_opt RPAREN statement
      { For (`Var $4, $6, $8, $10, $1) }
  | FOR LPAREN left_hand_side_expression IN expression RPAREN statement
      { ForIn (`Expr $3, $5, $7, $1) }
  | FOR LPAREN VAR variable_declaration_no_in IN expression RPAREN statement
      { ForIn (`Var $4, $6, $8, $1) }

continue_statement:
    CONTINUE auto_semi
      { Continue (None, $1) }
  | CONTINUE identifier auto_semi
      { Continue (Some (fst $2), $1) }

break_statement:
    BREAK auto_semi
      { Break (None, $1) }
  | BREAK identifier auto_semi
      { Break (Some (fst $2), $1) }

return_statement:
    RETURN auto_semi
      { Return (None, $1) }
  | RETURN expression auto_semi
      { Return (Some $2, $1) }

with_statement:
    WITH LPAREN expression RPAREN statement     { With ($3, $5, $1) }

switch_statement:
    SWITCH LPAREN expression RPAREN case_block  { Switch ($3, $5, $1) }

case_block:
  | LBRACE case_clauses default_clause_opt case_clauses RBRACE
      { $2 @ $3 @ $4 }

case_clauses:
                                { [] }
  | case_clauses case_clause    { $1 @ [$2] }

case_clause:
    CASE expression COLON statement_list_opt    { (Some $2, $4) }

default_clause:
    DEFAULT COLON statement_list_opt            { (None, $3) }

default_clause_opt: default_clause { [$1] } | { [] }

labeled_statement:
    identifier COLON statement  { Labeled (fst $1, $3, snd $1) }

throw_statement:
    THROW expression auto_semi
      { Throw ($2, $1) }

try_statement:
    TRY block catch             { Try (fst $2, Some $3, None, $1) }
  | TRY block finally           { Try (fst $2, None, Some $3, $1) }
  | TRY block catch finally     { Try (fst $2, Some $3, Some $4, $1) }

catch:
    CATCH LPAREN identifier RPAREN block        { (fst $3, fst $5) }

finally:
    FINALLY block       { fst $2 }

debugger_statement:
    DEBUGGER auto_semi       { Debugger $1 }

function_declaration:
    FUNCTION identifier LPAREN formal_parameter_list_opt RPAREN block
      { FunctionDecl (fst $2, $4, fst $6, $1) }

function_expression:
    FUNCTION identifier_opt LPAREN formal_parameter_list_opt RPAREN block
      { Function ($2, $4, fst $6, $1) }

identifier_opt: identifier { Some (fst $1) } | { None }

formal_parameter_list:
    identifier                                  { [fst $1] }
  | formal_parameter_list COMMA identifier      { $1 @ [fst $3] }

formal_parameter_list_opt: formal_parameter_list { $1 } | { [] }

program:
    statement_list EOF  { $1 }

%%
