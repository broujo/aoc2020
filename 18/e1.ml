let test_input =
  "1 + 2 * 3 + 4 * 5 + 6
1 + (2 * 3) + (4 * (5 + 6))
2 * 3 + (4 * 5)
5 + (8 * 3 + 9 + 3 * 4 * 3)
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

module Parser = Parser1

let run input =
  let lexbuf = Lexing.from_string input in

  try
    Parser.main Lexer.token lexbuf
    |> CCList.fold_left (+) 0
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg; failwith "Error"
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf); failwith "Error"

let () =
  assert (run test_input == 71 + 51 + 26 + 437 + 12240 + 13632);
  let input = Helpers.input 18 in
  print_int (run (Lwt_main.run input))
