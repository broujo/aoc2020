let init_queue pre l =
  let q,tl = CCList.take_drop pre l in
  (CCFQueue.of_list q), tl

let check q i =
  let s = CCFQueue.to_seq q in
  OSeq.combinations 2 s
  |> OSeq.exists (fun l -> CCList.fold_left (+) 0 l = i)

let solve l =
  let rec aux l prev (ac1,ac3) =
    match l with
    | [] -> ac1,(ac3+1)
    | t :: q ->
        if t - prev = 1
        then aux q t ((ac1+1),ac3)
        else aux q t (ac1,(ac3+1))
  in aux l 0 (0,0)
  
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
  let a1,a3 = 
    input
    |> CCString.lines
    |> CCList.map int_of_string
    |> CCList.sort compare
    |> solve
  in
  print_int a1;
  print_string "\n";
  print_int a3;
  print_string "\n";
  a1*a3


let () =
  assert (run test_input == 220);
  let input = Helpers.input 10 in
  print_int (run (Lwt_main.run input))
