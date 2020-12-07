let rec count_trees m li la acc =
  if (CCArray.length m) <= li
  then acc
  else begin
    let nacc =
      if m.(li).(la) == '#'
      then acc + 1
      else acc in
    count_trees m (li+1) ((la+3) mod (CCArray.length m.(0))) nacc
  end

let solve entries =
  count_trees entries 0 0 0

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
  assert ((run test_input) == 7);
  let input = Helpers.input 3 in
  print_int (run (Lwt_main.run input))
