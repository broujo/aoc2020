let solve l =
  let rec aux l prev oneb p1 p2 p3 accg =
    match l with
    | [] -> CCList.fold_left ( * ) 1 (p1::accg)
    | t :: q ->
        if t - prev = 1
        then aux q t true (p1+p2+p3) p1 p2 accg
        else if t - prev = 2 && oneb
        then aux q t false (p1+p2) 1 0 accg
        else aux q t false 1 0 0 (p1 :: accg)
  in aux l 0 false 1 0 0 []
  
let test_input =
  "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"

let run input =
  input
  |> CCString.lines
  |> CCList.map int_of_string
  |> CCList.sort compare
  |> solve

let () =
  assert (run test_input == 19208);
  let input = Helpers.input 10 in
  print_int (run (Lwt_main.run input))
