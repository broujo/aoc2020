open Angstrom

(* Policy and password parsing *)
type policy = { l : int; u: int; c: char }
type password = string
type entry = { policy : policy; password: password}

let policy l u c = { l; u; c}
let entry policy password = { policy; password}

let is_digit =
    function '0' .. '9' -> true | _ -> false

let digits =
  lift int_of_string
    (take_while1 is_digit)

let parse_policy =
  lift3 policy
    (digits <* char '-')
    (digits <* char ' ')
    (any_char <* string ": ")

let parse_entry =
  lift2 entry
    parse_policy
    (take_while1 (fun _ -> true))
    
let parse entry =
  let r = parse_string ~consume:All parse_entry entry
  in CCResult.get_or_failwith r

