open Parser

type marked_op =
  { op: op;
    mutable m: bool; }

let init_op op = { op; m=false }

type program =
  { mutable ip: int;
    mutable acc: int;
    ops: marked_op array; }

let init_program ops = { ip = 0; acc = 0; ops }

(* dirty side effect rec *)
let rec run_program p =
  match p.ops.(p.ip) with
  | {m = true; op = _} -> p.acc
  | {op = NOP _; m = _} -> begin
      p.ops.(p.ip).m <- true;
      p.ip <- p.ip + 1;
      run_program p
    end
  | {op = ACC n; m = _} -> begin
      p.ops.(p.ip).m <- true;
      p.ip <- p.ip + 1;
      p.acc <- p.acc + n;
      run_program p
    end
  | {op = JMP n; m = _} -> begin
      p.ops.(p.ip).m <- true;
      p.ip <- p.ip + n;
      run_program p
    end

let test_input =
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"

let run input =
  input
  |> parse_program
  |> CCList.map init_op
  |> CCArray.of_list
  |> init_program
  |> run_program


let () =
  assert ((run test_input) == 5);
  let input = Helpers.input 8 in
  print_int (run (Lwt_main.run input))
