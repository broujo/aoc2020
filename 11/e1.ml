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

let seatl_to_str l =
  CCList.fold_left (fun acc col ->
    acc
    ^ (CCList.fold_left
         (fun acc s -> acc ^ (seat_to_str s))
         ""
         col)
    ^ "\n")
  ""
  l
  
module C = struct
  type t = Int.t * Int.t
  let compare (x0,y0) (x1,y1) =
    match Int.compare x0 x1 with
    | 0 -> Int.compare y0 y1
    | c -> c
end

module M = CCMap.Make(C)


let mark_occupied i j m =
  M.update
   (i,j)
   (fun e -> match e with
    | None -> Some 1
    | Some x -> Some (x+1))
   m

(* mark_neigh_f f m i j will call f i' j' m on each neighbours i',j' of i,j *)
let mark_neigh_f f m i j =
  CCList.fold_left
    (fun m i' ->
      CCList.fold_left 
        (fun m j' ->
          if i' = i && j' = j
          then m
          else f i' j' m)
        m
        (CCList.range (j-1) (j+1)))
    m
    (CCList.range (i-1) (i+1))

let mark_neigh m s i j =
  match s with
  | Not_seat | Empty -> m
  | Occupied -> mark_neigh_f mark_occupied m i j
  
let make_neighmap l =
  CCList.foldi
    (fun m i col ->
      CCList.foldi (fun m j s -> mark_neigh m s i j) m col)
    M.empty
    l
    
let seat_change s i j m =
  match s with
  | Not_seat -> Not_seat
  | Empty ->
      begin match M.get (i,j) m with
      | None -> Occupied
      | _ -> Empty
      end
  | Occupied ->
      begin match M.get (i,j) m with
      | Some x when x >= 4 -> Empty
      | _ -> Occupied
      end

let step l =
  let m = make_neighmap l in
  CCList.mapi
    (fun i col ->
      CCList.mapi
        (fun j s -> seat_change s i j m)
        col)
    l

let count_occupied l =
  CCList.map (fun l ->
    CCList.count (fun x ->
      match x with
      | Occupied -> true
      | _ -> false)
    l)
  l
  |>CCList.fold_left (+) 0

let find_repeat l =
  let rec aux l prev =
    if l = prev
    then prev
    else aux (step l) l in
  aux l []

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
  |> find_repeat
  |> count_occupied

let () =
  assert (run test_input == 37);
  let input = Helpers.input 11 in
  print_int (run (Lwt_main.run input))
