open Parser

let check entry =
  let n = CCString.fold
    (fun a c -> if c == entry.policy.c then a+1 else a)
    0
    entry.password in
  (n >= entry.policy.l) && (n <= entry.policy.u)

let solve entries =
  CCList.count check entries

let test_input = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"

let run input =
  input
  |> CCString.lines
  |> CCList.map parse
  |> solve

let () =
  assert ((run test_input) == 2);
  let input = Helpers.input 2 in
  print_int (run (Lwt_main.run input))
