open Ast

let global_ctx = Hashtbl.create 100

type ident_lookup =
  | TypeIdent of (ident * typ) list
  | AssumedIdent of typ
  | DefinedIdent of typ * term

let handle_command cmd = match cmd with
  | AssumCom (id, tp) ->
      if valid_type global_ctx tp
        then Hashtbl.add global_ctx id (AssumedIdent tp)
        else raise (InvalidInputError "Not a well-formed type in the global context.")
  | DefCom (id, tp, tm) ->
      if valid_term global_ctx tp tm
        then Hashtbl.add global_ctx id (DefinedIdent (tp, tm))
        else raise (InvalidInputError "Not a well-formed type and/or term in the global context.")
  | IndDefCom (id, constructors) ->
      if valid_ind_def global_ctx constructors
        then
             begin Hashtbl.add global_ctx id (TypeIdent constructors);
             iter (fun (c_id, c_tp) -> Hashtbl.add global_ctx c_id (AssumedIdent c_tp)) constructors
             end
        else raise (InvalidInputError "Invalid inductive type definition.")

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      try
        let _ = Parser.main Lexer.token lexbuf in
          (handle_command cmd;
          flush stdout
      with Stdlib.Parsing.Parse_error ->
        print_string "Invalid syntax.\n";
        flush stdout
    done
  with Lexer.Eof ->
    exit 0