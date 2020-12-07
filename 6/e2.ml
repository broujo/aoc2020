
let test_input =
  "abc

a
b
c

ab
ac

a
a
a
a

b"

module S = CCSet.Make(CCChar)

let all_q = S.of_list (CCString.to_list "abcdefghijkqlmnopqrstuvwxyz")

let run input =
  input
  |> CCString.split ~by:"\n\n"
  |> CCList.map (CCString.lines)
  |> CCList.map (fun l ->
      l
      |> CCList.map CCString.to_list
      |> CCList.map S.of_list
      |> CCList.fold_left S.inter all_q
      |> S.cardinal )
  |> CCList.fold_left (+) 0

let () =
  assert ((run test_input) == 6);
  let input = Helpers.input 6 in
  print_int (run (Lwt_main.run input))

