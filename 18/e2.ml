module Parser = Parser2
let run input =
  let lexbuf = Lexing.from_string input in

  try
    Parser2.main Lexer.token lexbuf
    |> CCList.fold_left (+) 0
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg; failwith "Error"
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf); failwith "Error"

let () =
  let input = Helpers.input 18 in
  print_int (run (Lwt_main.run input))
