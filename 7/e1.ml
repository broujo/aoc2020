open Parser

module M = CCMap.Make(CCString)

let add_in_map e o =
  match o with
  | Some l -> Some (e :: l)
  | None -> Some ([e])

let make_reverse m =
  M.fold
    (fun key l acc ->
      CCList.fold_left (fun m (_,color) -> M.update color (add_in_map key) m)
        acc
        l)
    m
    M.empty

module S = CCSet.Make(CCString)
let count bag rev_m =
  let rec aux rev_m bag =
    match M.get bag rev_m with
    | None -> (S.add bag S.empty)
    | Some(l) ->
      l
      |> CCList.map (aux rev_m)
      |> CCList.fold_left (S.union) (S.add bag S.empty) in
  S.remove bag (aux rev_m bag)
  |> S.cardinal

let test_input =
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."


let run input =
  input
  |> CCString.lines
  |> CCList.map parse_rule
  |> M.of_list
  |> make_reverse
  |> count "shiny gold"

let () =
  assert ((run test_input) == 4);
  let input = Helpers.input 7 in
  print_int (run (Lwt_main.run input))
