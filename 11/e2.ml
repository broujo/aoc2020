type seat =
  | Empty
  | Occupied
  | Not_seat

let to_seat s =
  match s with
  | '.' -> Not_seat
  | '#' -> Occupied
  | 'L' -> Empty
  | _ -> failwith "Wrong input"

let to_seat_list l =
  CCList.map to_seat l

let seat_to_str s =
  match s with
  | Not_seat -> "."
  | Occupied -> "#"
  | Empty -> "L"

let seat_to_chr s =
  match s with
  | Not_seat -> '.'
  | Occupied -> '#'
  | Empty -> 'L'

let seatl_to_str l =
  CCList.fold_left (fun acc col ->
    acc
    ^ CCString.of_list col
    ^ "\n")
  ""
  l

let seata_to_str a =
  CCArray.fold (fun acc col ->
    acc
    ^ CCString.of_array col
    ^ "\n")
  ""
  (CCArray.map (CCArray.map seat_to_chr) a)
  
let directions =
  [(1, 1); (1, 0); (1, -1);
   (0, -1); (0, 1); 
   (-1, -1); (-1, 0); (-1, 1)]

let do_incr m i j = m.(i).(j) <- m.(i).(j) + 1

let rec mark_visible_dir (i, j) (di, dj) a m =
  let t =
    try Some (a.(i + di).(j + dj) = Not_seat)
    with Invalid_argument _ -> None in
  match t with
  | Some true ->
      begin
        do_incr m (i + di) (j + dj);
        mark_visible_dir ((i + di),(j + dj)) (di, dj) a m
      end
  | None -> ()
  | Some false -> do_incr m (i + di) (j + dj)

let mark_visible a (i, j) m =
  match a.(i).(j) with
  | Not_seat | Empty -> ()
  | Occupied -> CCList.iter (fun x -> mark_visible_dir (i, j) x a m) directions

let make_visible_map a =
  let m = CCArray.map (Array.map (fun _ -> 0)) a in
  CCArray.iteri
    (fun i _ -> CCArray.iteri (fun j _ -> mark_visible a (i,j) m) a.(i))
    a;
  m

let seat_change a (i, j) m =
  match a.(i).(j) with 
  | Not_seat -> ()
  | Empty ->
      if m.(i).(j) == 0
      then a.(i).(j) <- Occupied
  | Occupied ->
      if m.(i).(j) >= 5
      then a.(i).(j) <- Empty

let step a =
  print_string (seata_to_str a);
  print_string ("\n");
  let m = make_visible_map a in
  CCArray.iteri
    (fun i _ -> CCArray.iteri (fun j _ -> seat_change a (i, j) m) a.(i))
    a

let count_occupied a =
  CCArray.fold (fun acc a ->
    (CCArray.fold (fun acc x ->
      match x with
      | Occupied -> acc + 1
      | _ -> acc)
    0
    a)+acc)
  0
  a

let find_repeat a =
  let rec aux a prev =
    if a = prev
    then prev
    else
      begin
        let b = CCArray.map CCArray.copy a
        in step b; aux b a
      end in
  aux a CCArray.empty

let test_input =
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"

let run input =
  input
  |> CCString.lines
  |> CCList.map CCString.to_list
  |> CCList.map to_seat_list
  |> CCList.map CCArray.of_list
  |> CCArray.of_list
  |> find_repeat
  |> count_occupied

let () =
  assert (run test_input == 26);
  let input = Helpers.input 11 in
  print_int (run (Lwt_main.run input))
