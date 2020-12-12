let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

type direction =
  | North
  | South
  | East
  | West

type instruction =
  | Dir of direction*int
  | TurnL of int
  | Forward of int

let parse_instruction i =
  let ins,value = i.[0], int_of_string(CCString.sub i 1 (CCString.length i - 1)) in
  match ins with
  | 'N' -> Dir (North,value)
  | 'S' -> Dir (South,value)
  | 'E' -> Dir (East,value)
  | 'W' -> Dir (West,value)
  | 'L' -> TurnL value
  | 'R' -> TurnL (modulo (-value) 360)
  | 'F' -> Forward value
  | _ -> failwith "unknown instruction"

type state = {x: int; y: int; d: direction}

let turn90 d =
  match d with
  | North -> West
  | West -> South
  | South -> East
  | East -> North
 
let turn v d =
  match v with
  | 0 -> d
  | 90 -> turn90 d
  | 180 -> turn90 (turn90 d)
  | 270 -> turn90 (turn90 (turn90 d))
  | 360 -> d
  | _ -> failwith "invalid value"

let movdir d v s =
  match d with
  | North -> { s with x = s.x + v }
  | South -> { s with x = s.x - v }
  | West -> { s with y = s.y - v }
  | East -> { s with y = s.y + v }

let move s i =
  match i with
  | Dir (d, v) -> movdir d v s
  | TurnL v -> { s with d = (turn v s.d ) }
  | Forward v -> movdir s.d v s

let manhattan d = (Int.abs d.x) + (Int.abs d.y)

let test_input =
  "F10
N3
F7
R90
F11"

let run input =
  input
  |> CCString.lines
  |> CCList.map parse_instruction
  |> CCList.fold_left move { x = 0; y = 0; d = East }
  |> manhattan

let () =
  assert (run test_input == 25);
  let input = Helpers.input 12 in
  print_int (run (Lwt_main.run input))
