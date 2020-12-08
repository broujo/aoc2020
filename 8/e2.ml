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
  if p.ip > (CCArray.length p.ops) then None
  else if p.ip == (CCArray.length p.ops) then Some p.acc
  else begin
    match p.ops.(p.ip) with
    | {m = true; op = _} -> None
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
  end

let print_program l =
  CCList.iter
    (fun x -> Printf.printf ("%s\n") (op_to_s x))
    l;
  print_string "  **END**  \n";
  l

let run_program_list l =
  l
  |> CCList.map init_op
  |> CCArray.of_list
  |> init_program
  |> run_program

let solve lp =
  let rec aux start fin =
    match fin with
    | [] -> raise Not_found
    | (NOP n) :: r -> begin
        let acc = run_program_list (start @ (JMP n :: r)) in
        match acc with
        | None -> aux (start @ [NOP n]) r
        | Some x -> x end
    | (JMP n) :: r -> begin
        let acc = run_program_list (start @ (NOP n :: r)) in
        match acc with
        | None -> aux (start @ [JMP n]) r
        | Some x -> x end
    | (ACC n) :: r -> aux (start @ [ACC n]) r
  in
  aux [] lp

let test_input =
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
"

let run input =
  input
  |> parse_program
  |> solve


let () =
  assert ((run test_input) == 8);
  let input = Helpers.input 8 in
  print_int (run (Lwt_main.run input))
