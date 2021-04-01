open Ast

let global_ctx = Hashtbl.create 100

type ident_lookup =
  | TypeIdent of (ident * typ) list
  | AssumedIdent of typ
  | DefinedIdent of typ * term

let lookup id = 

let rec handle_command = function
  | AssumCom (id, tp) -> 

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let cmd = Parser.main Lexer.token lexbuf in
        handle_command cmd;;
        flush stdout
    done
  with Lexer.Eof ->
    exit 0