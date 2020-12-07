let check a b c =
  if a + b + c == 2020
  then Some(a,b,c)
  else None
 
let solve entries =
  CCList.find_map
    (fun e ->
       CCList.find_map (
         fun f -> CCList.find_map (check e f) entries)
       entries)
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
  | Some (a, b, c) -> a * b * c

let () =
  assert ((run test_input) == 241861950);
  let input = Helpers.input 1 in
  print_int (run (Lwt_main.run input))
