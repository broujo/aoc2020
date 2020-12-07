type slope = { x : int; y : int }
let slope x y = { x ; y }

let rec count_trees m slope li la acc =
  if (CCArray.length m) <= li
  then acc
  else begin
    let nacc =
      if m.(li).(la) == '#'
      then acc + 1
      else acc in
    count_trees m slope (li + slope.y) ((la + slope.x) mod (CCArray.length m.(0))) nacc
  end

let solve entries =
  [ slope 1 1;
    slope 3 1;
    slope 5 1;
    slope 7 1;
    slope 1 2; ]
  |> CCList.map (fun slope -> count_trees entries slope 0 0 0)
  |> CCList.fold_left CCInt.mul 1

let test_input =
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"


let run input =
  input
  |> CCString.lines
  |> CCList.map CCString.to_array
  |> CCArray.of_list
  |> solve

let () =
  assert ((run test_input) == 336);
  let input = Helpers.input 3 in
  print_int (run (Lwt_main.run input))
