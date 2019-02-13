{ open Parser }

let num = '-'?['0'-'9']+
let ident = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*
let separator = [' ''\t''\n']+

rule tokenize = parse
        | separator+            { tokenize lexbuf }
(* sepecial characters *)
        | "["                   { LBRACKET }
        | "]"                   { RBRACKET }
        | "("                   { LPAREN }
        | ")"                   { RPAREN }
        | ";"                   { SEMICOLON }
        | ":"                   { COLON }
        | ","                   { COMMA }
        | "*"                   { STAR }
        | "->"                  { ARROW }
(* reserved words *)
        | "ECHO"                { ECHO }
        | "FUN"                 { FUN }
        | "REC"                 { REC }
        | "CONST"               { CONST }
        | "true"                { TRUE }
        | "false"               { FALSE }
        | "not"                 { NOT }
        | "and"                 { AND }
        | "or"                  { OR }
        | "bool"                { BOOL }
        | "eq"                  { EQ }
        | "lt"                  { LT }
        | "add"                 { ADD }
        | "sub"                 { SUB }
        | "mul"                 { MUL }
        | "div"                 { DIV }
        | "int"                 { INT }
        | "if"                  { IF }
(* identifiers *)
        | ident as name         { IDENT (name) }
(* numeric values *)
        | num as numstr         { NUMVAL (int_of_string numstr) }
