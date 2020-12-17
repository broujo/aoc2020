type cube =
  | Active
  | Not_active

let to_seat s =
  match s with
  | '.' -> Not_active
  | '#' -> Active
  | _ -> failwith "Wrong input"

let filter_active s =
  match to_seat s with
  | Active -> Some Active
  | _ -> None

module C = struct
  type t = Int.t * Int.t * Int.t
  let compare (x0,y0,z0) (x1,y1,z1) =
    match Int.compare x0 x1 with
    | 0 ->
        begin match Int.compare y0 y1 with
        | 0 -> Int.compare z0 z1
        | c -> c end
    | c -> c
end

module P = CCMap.Make(C)

let pocket_add_row z p x l =
  CCList.foldi (fun p y e -> P.add (x, y, z) e p) p l

let pocket_init l =
  let z = 0 and p = P.empty in
  CCList.foldi (pocket_add_row z) p l
  |> P.filter (fun _ a -> a = Active)

let to_cube_list l =
  CCList.map (fun l -> CCList.map to_seat l) l

let directions_ =
  [(1, 1, 1);   (1, 1, 0);   (1, 1, -1);
   (1, 0, 1);   (1, 0, 0);   (1, 0, -1);
   (1, -1, 1);  (1, -1, 0);  (1, -1, -1);
   (0, 1, 1);   (0, 1, 0);   (0, 1, -1);
   (0, 0, 1);                (0, 0, -1);
   (0, -1, 1);  (0, -1, 0);  (0, -1, -1);
   (-1, 1, 1);  (-1, 1, 0);  (-1, 1, -1);
   (-1, 0, 1);  (-1, 0, 0);  (-1, 0, -1);
   (-1, -1, 1); (-1, -1, 0); (-1, -1, -1)]

let directions =
  let open CCList.Infix in
  CCList.flat_map (fun i ->
    CCList.flat_map (fun j ->
      CCList.map (fun k ->
        (i,j,k)) (-1 -- 1)
      ) (-1 -- 1)
    ) (-1 -- 1)
  |> CCList.filter (fun (a,b,c) -> (a,b,c) <> (0,0,0))

let do_incr m coord =
  P.update coord
    (fun x -> match x with
      | Some x -> Some (x + 1)
      | None -> Some 1)
    m

let mark_neighbour (i, j, k) m (di, dj, dk) =
  do_incr m (i + di, j + dj, k + dk)

let mark_neighbours coord cube m =
  match cube with
  | Active -> CCList.fold_left (mark_neighbour coord) m directions
  | _ -> m

let cube_change p coord count new_p =
  match P.get coord p with
  | Some Active ->
      begin match count with
      | 2 | 3 -> P.add coord Active new_p
      | _ -> new_p end
  | _ ->
      begin match count with
      | 3 -> P.add coord Active new_p
      | _ -> new_p end

let step p =
  let m = P.fold mark_neighbours p P.empty in
  P.fold (cube_change p) m P.empty

let rec repeat_step n p =
  match n with
  | 0 -> p
  | _ -> repeat_step (n-1) (step p)

let test_input =
  ".#.
..#
###"

let run input =
  input
  |> CCString.lines
  |> CCList.map CCString.to_list
  |> to_cube_list
  |> pocket_init
  |> repeat_step 6
  |> P.filter (fun _ a -> a = Active)
  |> P.cardinal

let () =
  assert (run test_input == 112);
  let input = Helpers.input 17 in
  print_int (run (Lwt_main.run input))
