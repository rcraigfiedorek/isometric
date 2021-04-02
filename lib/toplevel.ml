(*
let global_ctx = Hashtbl.create 100

type ident_lookup =
  | TypeIdent of (ident * typ) list
  | AssumedIdent of typ
  | DefinedIdent of typ * term

let handle_command cmd = match cmd with
  | AssumCom (id, tp) ->
  | DefCom (id, tp, tm) ->
  | IndDefCom (id, constructors) ->
*)
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      try
        let _ = Parser.main Lexer.token lexbuf in
          (* handle_command cmd;; *)
          flush stdout
      with Stdlib.Parsing.Parse_error ->
        print_string "Invalid syntax.\n";
        flush stdout
    done
  with Lexer.Eof ->
    exit 0