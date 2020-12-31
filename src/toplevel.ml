let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let _ = Parser.main Lexer.token lexbuf in
        flush stdout
    done
  with Lexer.Eof ->
    exit 0