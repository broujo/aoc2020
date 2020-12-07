let check a b =
  if a + b == 2020
  then Some(a,b)
  else None
 
let solve entries =
  CCList.find_map
    (fun e -> CCList.find_map (check e) entries)
    entries

let test_input = "1721
979
366
299
675
1456"

let run input = 
  let entries =
    input
    |> CCString.lines
    |> CCList.map int_of_string in
  match solve entries with
  | None -> raise Not_found
  | Some (a,b) -> a * b

let () =
  assert ((run test_input) == 514579);
  let input = Helpers.input 1 in
  print_int (run (Lwt_main.run input))

