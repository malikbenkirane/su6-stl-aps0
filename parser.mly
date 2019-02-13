%{
    open Ast
%}

%token                  ECHO
%token                  CONST, FUN, REC
%token                  LPAREN,   RPAREN
%token                  LBRACKET, RBRACKET
%token                  ARROW, STAR
%token                  IF
%token                  COLON, SEMICOLON, COMMA
%token                  NOT, AND, OR
%token                  EQ, LT
%token                  ADD, SUB, MUL, DIV
%token                  BOOL, INT
%token                  TRUE, FALSE
%token<string>          IDENT
%token<int>             NUMVAL

%type<Ast.prog>         prog
%type<Ast.cmd list>     cmds
%type<Ast.stat>         stat
%type<Ast.dec>          dec
%type<Ast.fundec>       fundec
%type<Ast.t>            t
%type<Ast.t list>       types
%type<Ast.arg>          arg
%type<Ast.arg list>     args
%type<Ast.expr>         expr
%type<Ast.expr list>    exprs
%type<Ast.oprim>        oprim

%start prog

%%

prog    :       LBRACKET cmds RBRACKET {$2}

cmds    :       stat                            { [Stat($1)] }
        |       dec SEMICOLON cmds              { Dec($1)::$3 }
        |       stat SEMICOLON cmds             { Stat($1)::$3 }

stat    :       ECHO expr                       { Echo($2) }

dec     :       CONST IDENT t expr              { Const($2, $3, $4) }
        |       FUN fundec                      { Fun($2) }
        |       FUN REC fundec                  { FunRec($3) }

fundec  :       IDENT t LBRACKET args RBRACKET expr
                                                { ($1, $2, $4, $6) }

t       :       INT                             { Int }
        |       BOOL                            { Bool }
        |       LPAREN types ARROW t RPAREN     { Rel($2, $4) }

types   :       t                               { [$1] }
        |       t STAR types                    { $1::$3 }

args    :       arg                             { [$1] }
        |       arg COMMA args                  { $1::$3 }

arg     :       IDENT COLON t                   { ($1, $3) }

expr    :       TRUE                            { True }
        |       FALSE                           { False }
        |       NUMVAL                          { Num($1) }
        |       IDENT                           { Ident($1) }
        |       LPAREN oprim exprs RPAREN       { Op($2, $3) }
        |       LBRACKET args RBRACKET expr     { Lambda($2, $4) }
        |       LPAREN expr exprs RPAREN        { App($2, $3) }
        |       LPAREN IF expr expr expr RPAREN { If($3, $4, $5) }

exprs   :       expr exprs                      { $1::$2 }
        |       expr                            { [$1] }

oprim   :       DIV { Div } | ADD { Add } | SUB { Sub } | MUL { Mul }
        |       NOT { Not } | AND { And } | OR { Or }
        |       EQ { Eq } | LT { Lt }
