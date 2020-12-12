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
type wp = {wx: int; wy: int}

let turn90 (x',y') = y', -x'
 
let turn wp v =
  let d = wp.wx, wp.wy in
  let dx, dy =
    begin match v with
    | 0 -> d
    | 90 -> turn90 d
    | 180 -> turn90 (turn90 d)
    | 270 -> turn90 (turn90 (turn90 d))
    | 360 -> d
    | _ -> failwith "invalid value"
    end in
  { wx = dx; wy = dy }

let movdir d v wp =
  match d with
  | North -> { wp with wx = wp.wx + v }
  | South -> { wp with wx = wp.wx - v }
  | West -> { wp with wy = wp.wy - v }
  | East -> { wp with wy = wp.wy + v }

let move (s,wp) i =
  match i with
  | Dir (d, v) -> (s,movdir d v wp)
  | TurnL v -> (s,turn wp v)
  | Forward v -> ({ s with x = (s.x + wp.wx * v); y = (s.y + wp.wy * v) },wp)

let manhattan (d,_) = (Int.abs d.x) + (Int.abs d.y)

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
  |> CCList.fold_left move ({ x = 0; y = 0; d = East },{ wx = 1; wy = 10})
  |> manhattan

let () =
  assert (run test_input == 286);
  let input = Helpers.input 12 in
  print_int (run (Lwt_main.run input))
