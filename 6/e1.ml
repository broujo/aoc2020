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

let run input =
  input
  |> CCString.split ~by:"\n\n"
  |> CCList.map (CCString.to_list)
  |> CCList.map (fun l ->
      l
      |> CCList.filter (fun x -> x != '\n')
      |> CCList.sort_uniq ~cmp:compare
      |> CCList.length)
  |> CCList.fold_left (+) 0

let () =
  assert ((run test_input) == 11);
  let input = Helpers.input 6 in
  print_int (run (Lwt_main.run input))

