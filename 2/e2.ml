open Parser

let check entry =
  (entry.password.[entry.policy.l - 1] == entry.policy.c)
  <> (entry.password.[entry.policy.u - 1] == entry.policy.c)

let solve entries =
  CCList.count check entries

let test_input =
  "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"

let run input =
  input
  |> CCString.lines
  |> CCList.map parse
  |> solve

let () =
  assert ((run test_input) == 1);
  let input = Helpers.input 2 in
  print_int (run (Lwt_main.run input))

