open Parser

type marked_op = { op: op; mutable m: bool; }

let init_op op = { op; m=false }

type program =
  { mutable ip: int;
    mutable acc: int;
    ops: marked_op array; }

module Node = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module Edge = struct
  type t = op
  let compare = compare
  let equal = (=)
  let default = NOP 0
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

module S = CCSet.Make(Node)

let program_to_graph l =
  let aux acc i op =
    match op with
    | NOP _ | ACC _ -> G.add_edge acc i (i+1)
    | JMP n -> G.add_edge acc i (i+n)
  in
  let g = G.add_vertex G.empty (G.V.create 0) in
  let g = CCList.foldi (fun acc i _ -> G.add_vertex acc (G.V.create (i+1))) g l in
  CCList.foldi aux g l

let accessible_from_end last g =
  let rec aux g todo seen =
    match todo with
    | [] -> seen
    | t :: q -> begin
        let preds = S.of_list (G.pred g t) in
        let todo' = S.union (S.diff preds seen) (S.of_list q) in
        aux g (S.to_list todo') (S.add t seen)
      end
  in
  aux g [last] (S.empty)

let init_program ops = { ip = 0; acc = 0; ops }

let do_nop p =
  p.ops.(p.ip).m <- true;
  p.ip <- p.ip + 1

let do_jmp p n =
  p.ops.(p.ip).m <- true;
  p.ip <- p.ip + n

let do_acc p n =
  p.ops.(p.ip).m <- true;
  p.ip <- p.ip + 1;
  p.acc <- p.acc + n

(* dirty side effect recusion *)
let rec run_program p s nm =
  if p.ip > (CCArray.length p.ops) then None
  else if p.ip == (CCArray.length p.ops) then Some p.acc
  else begin
    match p.ops.(p.ip) with
    | {m = true; op = _} -> None
    | {op = NOP n; m = _} -> begin
          if nm && S.mem (p.ip + n) s
          then (do_jmp p n; run_program p s false)
          else (do_nop p; run_program p s nm)
        end
    | {op = ACC n; m = _} -> begin
          do_acc p n;
          run_program p s nm
        end
    | {op = JMP n; m = _} -> begin
          if nm && S.mem (p.ip + 1) s
          then (do_nop p; run_program p s false)
          else (do_jmp p n; run_program p s nm)
        end
  end

let print_program l =
  print_string "  **PROGRAM**  \n";
  CCList.iter
    (fun x -> Printf.printf ("%s\n") (op_to_s x))
    l;
  print_string "  **END**  \n";
  l

let run_program_list l s nm =
  l
  |> CCList.map init_op
  |> CCArray.of_list
  |> init_program
  |> fun x -> run_program x s nm

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
  let l = parse_program input in
  let p = program_to_graph l in
  let s = accessible_from_end (CCList.length l) p in
  match run_program_list l s true with
  | None -> raise Not_found
  | Some x -> x

let () =
  assert ((run test_input) == 8);
  let input = Helpers.input 8 in
  print_int (run (Lwt_main.run input))
