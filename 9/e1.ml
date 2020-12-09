let init_queue pre l =
  let q,tl = CCList.take_drop pre l in
  (CCFQueue.of_list q), tl

let check q i =
  let s = CCFQueue.to_seq q in
  OSeq.combinations 2 s
  |> OSeq.exists (fun l -> CCList.fold_left (+) 0 l = i)

let solve pre l =
  let rec aux q l =
    match l with
    | [] -> raise Not_found
    | h :: tl ->
        if check q h
        then begin
          let _, q = CCFQueue.take_front_exn q in
          aux (CCFQueue.snoc q h) tl end
        else h
  in
  let q, l = init_queue pre l in aux q l
  
let test_input =
  "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
"

let run input pre =
  input
  |> CCString.lines
  |> CCList.map int_of_string
  |> solve pre


let () =
  assert ((run test_input 5) == 127);
  let input = Helpers.input 9 in
  print_int (run (Lwt_main.run input) 25)
