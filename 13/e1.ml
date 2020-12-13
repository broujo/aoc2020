let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

let parse_bus ts b =
  match b with
  | "x" -> None
  | _ -> let b = int_of_string b in Some(b, (b - (modulo ts b)))

let minpair2 (pa,a) (pb,b) =
  if a <= b
  then (pa, a)
  else (pb, b)

let test_input =
  "939
7,13,x,x,59,x,31,19"

let r_input = 
  "1002576
13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17"

let run input =
  let o =
    input
    |> CCString.lines in
  let ts,busses =
    match o with
    | ts :: busses :: [] -> ts,busses
    | _ -> failwith "bad input" in
  let ts = int_of_string ts in
  let bus,wait =
    busses
    |> CCString.split_on_char ','
    |> CCList.filter_map (parse_bus ts)
    |> CCList.fold_left minpair2 (1,11111111111) in
  bus * wait


let () =
  assert (run test_input == 295);
  let input = Helpers.input 13 in
  print_int (run (Lwt_main.run input))
