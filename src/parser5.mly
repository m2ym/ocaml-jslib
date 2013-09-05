%{
  open Ast
%}

/* Identifier */
%token <string>
    IDENTIFIER
%token
    GET SET
/* ReservedWord */
%token
    BREAK DO INSTANCEOF TYPEOF
    CASE ELSE NEW VAR
    CATCH FINALLY RETURN VOID
    CONTINUE FOR SWITCH WHILE
    DEBUGGER FUNCTION THIS WITH
    DEFAULT IF THROW
    DELETE IN TRY
/* FutureReservedWord */
%token
    CLASS ENUM EXTENDS SUPER
    CONST EXPORT IMPORT
    IMPLEMENTS LET PRIVATE PUBLIC
    INTERFACE PACKAGE PROTECTED STATIC
    YIELD
/* Punctuator */
%token
    LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
    DOT SEMI COMMA LT GT LE GE
    EQ2 NEQ EQ3 NEQ3
    PLUS MINUS MULT DIV MOD PLUS2 MINUS2
    LT2 GT2 GT3 AND OR XOR
    NOT NEG AND2 OR2 QUESTION COLON
    EQ PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ MOD_EQ LT2_EQ
    GT2_EQ GT3_EQ AND_EQ OR_EQ XOR_EQ
%token NULL TRUE FALSE
%token <[`Dec of string | `Hex of string]>
    NUMERIC
%token <string>
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
  | GET         { "get" }
  | SET         { "set" }

identifier_name:
    identifier  { $1 }
  | BREAK       { "break" }
  | DO          { "do" }
  | INSTANCEOF  { "instanceof" }
  | TYPEOF      { "typeof" }
  | CASE        { "case" }
  | ELSE        { "else" }
  | NEW         { "new" }
  | VAR         { "var" }
  | CATCH       { "catch" }
  | FINALLY     { "finally" }
  | RETURN      { "return" }
  | VOID        { "void" }
  | CONTINUE    { "continue" }
  | FOR         { "for" }
  | SWITCH      { "switch" }
  | WHILE       { "while" }
  | DEBUGGER    { "debugger" }
  | FUNCTION    { "function" }
  | THIS        { "this" }
  | WITH        { "with" }
  | DEFAULT     { "default" }
  | IF          { "if" }
  | THROW       { "throw" }
  | DELETE      { "delete" }
  | IN          { "in" }
  | TRY         { "try" }
  | CLASS       { "class" }
  | ENUM        { "enum" }
  | EXTENDS     { "extends" }
  | SUPER       { "super" }
  | CONST       { "const" }
  | EXPORT      { "export" }
  | IMPORT      { "import" }
  | IMPLEMENTS  { "implements" }
  | LET         { "let" }
  | PRIVATE     { "private" }
  | PUBLIC      { "public" }
  | INTERFACE   { "interface" }
  | PACKAGE     { "package" }
  | PROTECTED   { "protected" }
  | STATIC      { "static" }
  | YIELD       { "yield" }
  | NULL        { "null" }
  | TRUE        { "true" }
  | FALSE       { "false" }

literal:
    NULL        { `Null }
  | TRUE        { `Bool true }
  | FALSE       { `Bool false }
  | NUMERIC     { `Number $1 }
  | STRING      { `String $1 }
/* TODO RegularExpressionLiteral */

primary_expression:
    THIS                        { This }
  | identifier                  { Ident $1 }
  | literal                     { Literal $1 }
  | array_literal               { $1 }
  | object_literal              { $1 }
  | LPAREN expression RPAREN    { $2 }

array_literal:
    LBRACK RBRACK
      { Array [] }
  | LBRACK element_list elision_opt RBRACK
      { Array ($2) }

element_list:
    elision_opt assignment_expression
      { [$2] }
  | element_list COMMA elision_opt assignment_expression
      { $1 @ [$4] }

elision: COMMA {} | elision COMMA {}

elision_opt: elision {} | {}

object_literal:
    LBRACE RBRACE
      { Object [] }
  | LBRACE property_name_and_value_list RBRACE
      { Object $2 }
  | LBRACE property_name_and_value_list COMMA RBRACE
      { Object $2 }

property_name_and_value_list:
    property_assignment
      { [$1] }
  | property_name_and_value_list COMMA property_assignment
      { $1 @ [$3] }

property_assignment:
    property_name COLON assignment_expression
      { `Init ($1, $3) }
  | GET property_name LPAREN RPAREN block
      { `Get ($2, $5) }
  | SET property_name LPAREN identifier RPAREN block
      { `Set ($2, $4, $6) }

property_name:
    identifier          { `Ident $1 }
  | STRING              { `String $1 }
  | NUMERIC             { `Number $1 }

member_expression:
    primary_expression                          { $1 }
  | function_expression                         { $1 }
  | member_expression LBRACK expression RBRACK  { Member ($1, `Expr $3) }
  | member_expression DOT identifier_name       { Member ($1, `Ident $3) }
  | NEW member_expression arguments             { New ($2, $3) }

new_expression:
    member_expression                           { $1 }
  | NEW new_expression                          { New ($2, []) }

call_expression:
    member_expression arguments                 { Call ($1, $2) }
  | call_expression arguments                   { Call ($1, $2) }
  | call_expression LBRACK expression RBRACK    { Member ($1, `Expr $3) }
  | call_expression DOT identifier_name         { Member ($1, `Ident $3) }

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
  | left_hand_side_expression PLUS2     { Unary (`PostIncr, $1) }
  | left_hand_side_expression MINUS2    { Unary (`PostDecr, $1) }

unary_expression:
    postfix_expression                  { $1 }
  | DELETE unary_expression             { Unary (`Delete, $2) }
  | VOID unary_expression               { Unary (`Void, $2) }
  | TYPEOF unary_expression             { Unary (`TypeOf, $2) }
  | PLUS2 unary_expression              { Unary (`PreIncr, $2) }
  | MINUS2 unary_expression             { Unary (`PreDecr, $2) }
  | PLUS unary_expression               { Unary (`Plus, $2) }
  | MINUS unary_expression              { Unary (`Minus, $2) }
  | NEG unary_expression                { Unary (`Negate, $2) }
  | NOT unary_expression                { Unary (`Not, $2) }

multiplicative_expression:
    unary_expression
      { $1 }
  | multiplicative_expression MULT unary_expression
      { Binary (`Mult, $1, $3) }
  | multiplicative_expression DIV unary_expression
      { Binary (`Div, $1, $3) }
  | multiplicative_expression MOD unary_expression
      { Binary (`Mod, $1, $3) }

additive_expression:
    multiplicative_expression
      { $1 }
  | additive_expression PLUS multiplicative_expression
      { Binary (`Add, $1, $3) }
  | additive_expression MINUS multiplicative_expression
      { Binary (`Sub, $1, $3) }

shift_expression:
    additive_expression
      { $1 }
  | shift_expression LT2 additive_expression
      { Binary (`LShift, $1, $3) }
  | shift_expression GT2 additive_expression
      { Binary (`AShift, $1, $3) }
  | shift_expression GT3 additive_expression
      { Binary (`RShift, $1, $3) }

relational_expression:
    shift_expression
      { $1 }
  | relational_expression LT shift_expression
      { Binary (`Lt, $1, $3) }
  | relational_expression GT shift_expression
      { Binary (`Gt, $1, $3) }
  | relational_expression LE shift_expression
      { Binary (`LtE, $1, $3) }
  | relational_expression GE shift_expression
      { Binary (`GtE, $1, $3) }
  | relational_expression INSTANCEOF shift_expression
      { Binary (`InstanceOf, $1, $3) }
  | relational_expression IN shift_expression
      { Binary (`In, $1, $3) }

relational_expression_no_in:
    shift_expression
      { $1 }
  | relational_expression_no_in LT shift_expression
      { Binary (`Lt, $1, $3) }
  | relational_expression_no_in GT shift_expression
      { Binary (`Gt, $1, $3) }
  | relational_expression_no_in LE shift_expression
      { Binary (`LtE, $1, $3) }
  | relational_expression_no_in GE shift_expression
      { Binary (`GtE, $1, $3) }
  | relational_expression_no_in INSTANCEOF shift_expression
      { Binary (`InstanceOf, $1, $3) }

equality_expression:
    relational_expression
      { $1 }
  | equality_expression EQ2 relational_expression
      { Binary (`Eq, $1, $3) }
  | equality_expression NEQ relational_expression
      { Binary (`Neq, $1, $3) }
  | equality_expression EQ3 relational_expression
      { Binary (`Equal, $1, $3) }
  | equality_expression NEQ3 relational_expression
      { Binary (`NotEqual, $1, $3) }

equality_expression_no_in:
    relational_expression_no_in
      { $1 }
  | equality_expression_no_in EQ2 relational_expression_no_in
      { Binary (`Eq, $1, $3) }
  | equality_expression_no_in NEQ relational_expression_no_in
      { Binary (`Neq, $1, $3) }
  | equality_expression_no_in EQ3 relational_expression_no_in
      { Binary (`Equal, $1, $3) }
  | equality_expression_no_in NEQ3 relational_expression_no_in
      { Binary (`NotEqual, $1, $3) }

bitwise_AND_expression:
    equality_expression
      { $1 }
  | bitwise_AND_expression AND equality_expression
      { Binary (`BitAnd, $1, $3) }

bitwise_AND_expression_no_in:
    equality_expression_no_in
      { $1 }
  | bitwise_AND_expression_no_in AND equality_expression_no_in
      { Binary (`BitAnd, $1, $3) }

bitwise_XOR_expression:
    bitwise_AND_expression
      { $1 }
  | bitwise_XOR_expression XOR bitwise_AND_expression
      { Binary (`BitXor, $1, $3) }

bitwise_XOR_expression_no_in:
    bitwise_AND_expression_no_in
      { $1 }
  | bitwise_XOR_expression_no_in XOR bitwise_AND_expression_no_in
      { Binary (`BitXor, $1, $3) }

bitwise_OR_expression:
    bitwise_XOR_expression
      { $1 }
  | bitwise_OR_expression OR bitwise_XOR_expression
      { Binary (`BitOr, $1, $3) }

bitwise_OR_expression_no_in:
    bitwise_XOR_expression_no_in
      { $1 }
  | bitwise_OR_expression_no_in OR bitwise_XOR_expression_no_in
      { Binary (`BitOr, $1, $3) }

logical_AND_expression:
    bitwise_OR_expression
      { $1 }
  | logical_AND_expression AND2 bitwise_OR_expression
      { Binary (`And, $1, $3) }

logical_AND_expression_no_in:
    bitwise_OR_expression_no_in
      { $1 }
  | logical_AND_expression_no_in AND2 bitwise_OR_expression_no_in
      { Binary (`And, $1, $3) }

logical_OR_expression:
    logical_AND_expression
      { $1 }
  | logical_OR_expression OR2 logical_AND_expression
      { Binary (`Or, $1, $3) }

logical_OR_expression_no_in:
    logical_AND_expression_no_in
      { $1 }
  | logical_OR_expression_no_in OR2 logical_AND_expression_no_in
      { Binary (`Or, $1, $3) }

conditional_expression:
    logical_OR_expression
      { $1 }
  | logical_OR_expression QUESTION assignment_expression COLON assignment_expression
      { Ternary ($1, $3, $5) }

conditional_expression_no_in:
    logical_OR_expression_no_in
      { $1 }
  | logical_OR_expression_no_in QUESTION assignment_expression COLON assignment_expression_no_in
      { Ternary ($1, $3, $5) }

assignment_expression:
    conditional_expression
      { $1 }
  | left_hand_side_expression EQ assignment_expression
      { Assign (`Nop, $1, $3) }
  | left_hand_side_expression assignment_operator assignment_expression
      { Assign ($2, $1, $3) }

 assignment_expression_no_in:
    conditional_expression_no_in
      { $1 }
  | left_hand_side_expression EQ assignment_expression_no_in
      { Assign (`Nop, $1, $3) }
  | left_hand_side_expression assignment_operator assignment_expression_no_in
      { Assign ($2, $1, $3) }

assignment_operator:
    MULT_EQ     { `Mult }
  | DIV_EQ      { `Div }
  | MOD_EQ      { `Mod }
  | PLUS_EQ     { `Add }
  | MINUS_EQ    { `Sub }
  | LT2_EQ      { `LShift }
  | GT2_EQ      { `AShift }
  | GT3_EQ      { `RShift }
  | AND_EQ      { `BitAnd }
  | XOR_EQ      { `BitXor }
  | OR_EQ       { `BitOr }

expression:
    assignment_expression
      { $1 }
  | expression COMMA assignment_expression
      { Sequence ($1, $3) }

expression_opt: { None } | expression { Some $1 }

expression_no_in:
    assignment_expression_no_in
      { $1 }
  | expression_no_in COMMA assignment_expression_no_in
      { Sequence ($1, $3) }

statement:
    block                       { Block $1 }
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
    LBRACE RBRACE                       { [] }
  | LBRACE statement_list RBRACE        { $2 }

statement_list:
    statement                   { [$1] }
  | statement_list statement    { $1 @ [$2] }

statement_list_opt: statement_list { $1 } |  { [] }

variable_statement:
    VAR variable_declaration_list auto_semi
      { Var ($2) }

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
    identifier initializer_opt  { ($1, $2) }

variable_declaration_no_in:
    identifier initializer_no_in_opt  { ($1, $2) }

initializer_:
    EQ assignment_expression    { $2 }

initializer_opt: initializer_ { Some $1 } |  { None }

initializer_no_in:
    EQ assignment_expression_no_in      { $2 }

initializer_no_in_opt: initializer_no_in { Some $1 } | { None }

empty_statement:
    SEMI { Empty }

expression_statement:
    expression auto_semi { Expr $1 }

if_statement:
    IF LPAREN expression RPAREN statement ELSE statement
      { If ($3, $5, Some $7) } 
  | IF LPAREN expression RPAREN statement
      { If ($3, $5, None) } 

iteration_statement:
    DO statement WHILE LPAREN expression RPAREN auto_semi
      { Do ($2, $5) }
  | WHILE LPAREN expression RPAREN statement
      { While ($3, $5) }
  | FOR LPAREN SEMI expression_opt SEMI expression_opt RPAREN statement
      { For (`Nop, $4, $6, $8) }
  | FOR LPAREN expression_no_in SEMI expression_opt SEMI expression_opt RPAREN statement
      { For (`Expr $3, $5, $7, $9) }
  | FOR LPAREN VAR variable_declaration_list_no_in SEMI expression_opt SEMI expression_opt RPAREN statement
      { For (`Var $4, $6, $8, $10) }
  | FOR LPAREN left_hand_side_expression IN expression RPAREN statement
      { ForIn (`Expr $3, $5, $7) }
  | FOR LPAREN VAR variable_declaration_no_in IN expression RPAREN statement
      { ForIn (`Var $4, $6, $8) }

continue_statement:
    CONTINUE auto_semi
      { Continue None }
  | CONTINUE identifier auto_semi
      { Continue (Some $2) }

break_statement:
    BREAK auto_semi
      { Break None }
  | BREAK identifier auto_semi
      { Break (Some $2) }

return_statement:
    RETURN auto_semi
      { Return None }
  | RETURN expression auto_semi
      { Return (Some $2) }

with_statement:
    WITH LPAREN expression RPAREN statement     { With ($3, $5) }

switch_statement:
    SWITCH LPAREN expression RPAREN case_block  { Switch ($3, $5) }

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
    identifier_name COLON statement  { Labeled ($1, $3) }

throw_statement:
    THROW expression auto_semi
      { Throw ($2) }

try_statement:
    TRY block catch             { Try ($2, Some $3, None) }
  | TRY block finally           { Try ($2, None, Some $3) }
  | TRY block catch finally     { Try ($2, Some $3, Some $4) }

catch:
    CATCH LPAREN identifier RPAREN block        { ($3, $5) }

finally:
    FINALLY block       { $2 }

debugger_statement:
    DEBUGGER auto_semi       { Debugger }

function_declaration:
    FUNCTION identifier LPAREN formal_parameter_list_opt RPAREN block
      { FunctionDecl ($2, $4, $6) }

function_expression:
    FUNCTION identifier_opt LPAREN formal_parameter_list_opt RPAREN block
      { Function ($2, $4, $6) }

identifier_opt: identifier { Some $1 } | { None }

formal_parameter_list:
    identifier                                  { [$1] }
  | formal_parameter_list COMMA identifier      { $1 @ [$3] }

formal_parameter_list_opt: formal_parameter_list { $1 } | { [] }

program:
    statement_list EOF  { $1 }

%%
