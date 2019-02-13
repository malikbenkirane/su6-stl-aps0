open Ast

let rec pretty cmds = match cmds with
| cmd::rcmds -> "\n  " ^ pretty_cmd cmd ^ ";" ^ pretty rcmds
| [] -> "\n]"

and pretty_cmd cmd = match cmd with
| Stat stat -> pretty_stat stat
| Dec dec -> pretty_dec dec

and pretty_stat stat = match stat with
| Echo (expr) -> "ECHO " ^ pretty_expr expr

and pretty_dec dec = match dec with
| Const (name, t, expr) ->
     "CONST " ^ name ^ " " ^ (pretty_type t) ^ " " ^ (pretty_expr expr)
| Fun (fdec) -> "FUN " ^ pretty_fdec fdec
| FunRec (fdec) -> "FUN REC " ^ pretty_fdec fdec

and pretty_fdec fdec = match fdec with
| fname, t, args, e ->
     fname ^ " "
        ^ (pretty_type t)
        ^ " [" ^ (pretty_arglist args) ^ "] "
        ^ (pretty_expr e)

and pretty_arglist args = String.concat ", " (List.map pretty_arg args)

and pretty_arg arg = let name, t = arg in name ^ ":" ^ (pretty_type t)

and pretty_type t = match t with
| Int ->  "int"
| Bool ->  "bool"
| Rel (tlst, t) -> "(" ^ (pretty_tlist tlst) ^ " -> " ^ (pretty_type t) ^ ")"

and pretty_tlist tl = String.concat "*" (List.map pretty_type tl)

and pretty_expr expr = match expr with
| True -> "true"
| False -> "false"
| Num n -> string_of_int n
| Ident name -> name
| Op (op, exprs) ->
    "(" ^ (pretty_op op) ^ " " ^ (pretty_exprs exprs) ^ ")"
| Lambda (args, expr) ->  
    "[" ^ (pretty_arglist args) ^ "] " ^ (pretty_expr expr)
| App (expr, exprs) ->
    "(" ^ (pretty_expr expr) ^ " " ^ (pretty_exprs exprs) ^ ")"
| If (econd, etrue, efalse) -> 
    "(if "
        ^ (pretty_expr econd)
        ^ " " ^ (pretty_expr etrue)
        ^ " " ^ (pretty_expr efalse) 
        ^ ")"

and pretty_exprs exprs = String.concat " " (List.map pretty_expr exprs)

and pretty_op op = match op with
| Add -> "add"
| Sub -> "sub"
| Mul -> "mul"
| Div -> "div"
| Not -> "not"
| And -> "and"
| Or -> "or"
| Eq -> "eq"
| Lt -> "lt"

let pretty_print cmds = print_string ("[" ^ (pretty cmds) ^ "\n")
