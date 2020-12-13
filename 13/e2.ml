module BI = struct
  open Big_int
  let (/) = div_big_int
  let (+) = add_big_int
  let (-) = sub_big_int
  let ( * ) = mult_big_int

  let mod_inv a b =
    if eq_big_int unit_big_int b
    then b
    else
      let rec aux a b x0 x1 =
        if le_big_int a unit_big_int then x1 else
        if eq_big_int b zero_big_int then failwith "mod_inv" else
        aux b (mod_big_int a b) (x1 - (a / b) * x0) x0
      in
      let x = aux a b zero_big_int unit_big_int in
      if lt_big_int x zero_big_int then x + b else x

  let modulo = mod_big_int
end

let parse_bus (i,b) =
  match b with
  | "x" -> None
  | _ -> Some(Big_int.big_int_of_int i, Big_int.big_int_of_string b)

let bus_eq (i,a) (j,b) =
  let open BI in
  let a' = mod_inv a b in
  let ij = modulo ((j - i) * a' * a + i) (a * b) in
  (ij, a*b)

let test_input =
  "939
7,13,x,x,59,x,31,19"

let t2 =
  "939
17,x,13,19"

let t3 = 
  "1
67,x,7,59,61"

let t4 =
  "1
67,7,x,59,61"

let t5 =
  "1
1789,37,47,1889"

let r_input = 
  "1002576
13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17"

let run input =
  let o =
    input
    |> CCString.lines in
  let _,busses =
    match o with
    | ts :: busses :: [] -> ts,busses
    | _ -> failwith "bad input" in
  let i,a =
    busses
    |> CCString.split_on_char ','
    |> CCList.mapi (fun i e -> (i,e))
    |> CCList.filter_map parse_bus
    |> CCList.fold_left bus_eq (Big_int.unit_big_int, Big_int.unit_big_int) in
  Big_int.string_of_big_int BI.(a - i)



let () =
  assert (run test_input = "1068781");
  let input = Helpers.input 13 in
  print_string (run (Lwt_main.run input))
