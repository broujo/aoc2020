let to_seat_id s =
  s
  |> Str.global_replace (Str.regexp "[FL]") "0"
  |> Str.global_replace (Str.regexp "[BR]") "1"
  |> (^) "0b"
  |> int_of_string

let test_input = "FBFBBFFRLR"

let run input =
  input
  |> CCString.lines
  |> CCList.map to_seat_id
  |> CCList.fold_left (fun x y -> max x y) 0

let () =
  assert ((run test_input) == 357);
  let input = Helpers.input 5 in
  print_int (run (Lwt_main.run input))

