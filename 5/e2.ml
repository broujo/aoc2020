let to_seat_id s =
  s
  |> Str.global_replace (Str.regexp "[FL]") "0"
  |> Str.global_replace (Str.regexp "[BR]") "1"
  |> (^) "0b"
  |> int_of_string

let rec find_missing l =
  match l with
  | [] -> raise Not_found
  | _ :: [] -> raise Not_found
  | a :: b :: _ when b - a == 2 -> b - 1
  | _ :: t -> find_missing t

let run input =
  input
  |> CCString.lines
  |> CCList.map to_seat_id
  |> CCList.sort compare
  |> find_missing

let () =
  let input = Helpers.input 5 in
  print_int (run (Lwt_main.run input))

