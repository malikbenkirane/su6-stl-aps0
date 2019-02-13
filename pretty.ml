open Prettylib

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let cmds = Parser.prog Lexer.tokenize lexbuf in
    pretty_print cmds
