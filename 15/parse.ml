open Angstrom

type mask = {orm: int; andm: int}
type op =
  | Mask of mask
  | Assign of int*int

let mask_of_string s =
  let orms = "0b" ^ (CCString.replace ~sub:"X" ~by:"0" s) in
  let andms = "0b" ^ (CCString.replace ~sub:"X" ~by:"1" s) in
  { orm = (int_of_string orms); andm = (int_of_string andms)}

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
    (string "mem[" *> p_integer <* string "] = ")
    p_integer

let p_line =
  p_mask_line <|> p_assignement_line 

let p_program =
  (sep_by1 end_of_line p_line) <* (end_of_line <|> return ())

let parse_program r =
  CCResult.get_or_failwith (parse_string ~consume:All p_program r)
