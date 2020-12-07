open Angstrom

let is_digit =
  function '0' .. '9' -> true | _ -> false

let is_hex =
  function '0' .. '9' | 'a'..'f' -> true | _ -> false
 
let digits =
  lift int_of_string
    (take_while1 is_digit)

let hexs =
  (take_while1 is_hex)

type height =
  | In of int
  | Cm of int

let p_in =
  lift (fun x -> In(x))
    (digits <* (string "in"))

let p_cm =
  lift (fun x -> Cm(x))
    (digits <* (string "cm"))

let p_hgt =
  p_in <|> p_cm

let parse_hgt h =
  CCResult.to_opt (parse_string ~consume:All p_hgt h)

let p_hcl =
  char '#' *> hexs <* end_of_input >>= (fun h ->
  if (CCString.length h) == 6
  then return h
  else fail "wrong length")

let parse_hcl h =
  CCResult.to_opt (parse_string ~consume:All p_hcl h)

let p_pid =
  (take_while1 is_digit) <* end_of_input >>= (fun h ->
  if (CCString.length h) == 9
  then return h
  else fail "wrong length")

let parse_pid h =
  CCResult.to_opt (parse_string ~consume:All p_pid h)
