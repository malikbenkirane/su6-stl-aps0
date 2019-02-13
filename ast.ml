type prog = cmd list

and stat  = 
          | Echo of expr

and cmd   = 
          | Stat of stat
          | Dec of dec

and dec   = 
          | Const of (string * t * expr)
          | Fun of fundec
          | FunRec of fundec

and fundec = (string * t * arg list * expr)

and t     = 
          | Int
          | Bool
          | Rel of t list * t

and arg = string * t

and expr  =
          | True | False | Num of int | Ident of string
          | Op of (oprim * expr list)
          | Lambda of (arg list * expr)
          | App of (expr * expr list)
          | If of (expr * expr * expr)

and oprim = 
          | Add | Sub | Mul | Div
          | Not | And | Or
          | Eq | Lt
