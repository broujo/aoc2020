module M = CCMap.Make(CCInt)

let next_spoken e nth m =
  match M.get e m with
  | None -> 0
  | Some(x) -> nth - x

let solve l =
  
  let rec aux l nth prev next m =
    (* CCFormat.printf "%d\n" prev; *)
    let _ = prev in
    if nth mod 1000000 = 0
    then CCFormat.printf "%d/%d\n" (nth / 1000000) (30000000 / 1000000);
    if nth = 30000000
    then next
    else
      match l with
      | t :: q -> aux q (nth + 1) t (next_spoken t nth m) (M.add t nth m)
      | [] -> aux [] (nth + 1) next (next_spoken next nth m) (M.add next nth m)
  in aux l 1 0 0 M.empty

let run input =
  let r = 
    input
    |> CCString.split_on_char ','
    |> CCList.map int_of_string
    |> solve in
  CCFormat.printf "%s: %d\n" input r;
  r
  

let puzzle_input = "7,12,1,0,16,2"

let () =
(*  assert (run "0,3,6" == 175594);
(*  assert (run "1,3,2" == 1);
  assert (run "2,1,3" == 10);
  assert (run "1,2,3" == 27); *)
  assert (run "2,3,1" == 6895259); *)
(*  assert (run "3,2,1" == 438);
  assert (run "3,1,2" == 1836); *)
  print_int (run puzzle_input)
