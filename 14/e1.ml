module Parser = struct

  open Angstrom

  type bin =
    | One
    | Zero
    | X

  type mask = bin list
  type op =
    | Mask of mask
    | Assign of (bin list)*int

  let mask_of_string s =
    s
    |> CCString.to_list
    |> CCList.rev_map (function 'X' -> X | '1' -> One | '0' -> Zero | _ -> failwith "invalid mask")

  let bin_of_int i =
    let s = CCInt.to_string_binary i in
    CCString.sub s 2 (CCString.length s - 2)
    |> CCString.to_list
    |> CCList.rev_map (function '1' -> One | '0' -> Zero | _ -> failwith "invalid int")



  let is_bin =
    function '0' | '1' -> true | _ -> false

  let is_mask =
    function '0' | '1' | 'X' -> true | _ -> false

  let is_digit =
    function '0' .. '9' -> true | _ -> false

  let p_digits =
    take_while1 is_digit

  let p_masks =
    take_while1 is_mask

  let p_bins =
    take_while1 is_bin

  let p_integer =
    p_digits
    >>| int_of_string

  let p_intbin =
    p_integer
    >>| bin_of_int

  let p_mask =
    p_masks
    >>| mask_of_string 

  let p_binary =
    p_bins
    >>| (fun x -> int_of_string ("0b" ^ x))

  let p_mask_line =
    string "mask = " *> p_mask
    >>| (fun x -> Mask x)

  let p_assignement_line =
    lift2 (fun m i -> Assign(m,i))
      (string "mem[" *> p_intbin <* string "] = ")
      p_integer

  let p_line =
    p_mask_line <|> p_assignement_line 

  let p_program =
    (sep_by1 end_of_line p_line) <* (end_of_line <|> return ())

  let parse_program r =
    CCResult.get_or_failwith (parse_string ~consume:All p_program r)

end

open Parser

module M = CCMap.Make(Int)

let do_all_masks f mask baddr map =
  let rec aux mask baddr map acc =
    match mask, baddr with
    | [], [] -> f map acc
    | m :: mq, [] ->
        begin match m with
        | One -> aux mq baddr map ('1' :: acc)
        | Zero -> aux mq baddr map ('0' :: acc)
        | X -> let map = aux mq baddr map ('1' :: acc)
               in aux mq baddr map ('0' :: acc)
        end
    | m :: mq, v :: vq ->
        begin match m,v with
        | Zero,Zero -> aux mq vq map ('0' :: acc)
        | X,_ -> let map = aux mq vq map ('1' :: acc)
               in aux mq vq map ('0' :: acc)
        | _,_ -> aux mq vq map ('1' :: acc)
        end
    | _, _ -> failwith "mask should be bigger than addr"
  in aux mask baddr map []

let charlist_to_int cl =
  let s = CCString.of_list cl in
  int_of_string ("0b" ^ s)

let update_with_value v m addr =
  let k = charlist_to_int addr in
  M.update k (fun _ -> Some (v)) m

let apply (mask,map) e =
  match e with
  | Mask m -> (m, map)
  | Assign (addr,v) ->
      (mask, do_all_masks (update_with_value v) mask addr map)

let test_input =
  "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"

let run input =
  parse_program input
  |> List.fold_left apply ([],M.empty)
  |> fun (_,m) -> M.fold (fun _ a b -> a + b) m 0
  


let () =
  assert (run test_input == 208);
  let input = Helpers.input 14 in
  print_int (run (Lwt_main.run input))
