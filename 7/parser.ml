open Angstrom

let is_digit =
  function '0' .. '9' -> true | _ -> false

let digits =
  lift int_of_string
    (take_while1 is_digit)

let p_color =
  lift2 (fun a b -> (a ^ " " ^ b))
    ((take_while1 (fun x -> x <> ' ')) <* (char ' '))
    ((take_while1 (fun x -> x <> ' ')) <* (char ' '))

let p_bags =
  p_color <* (string "bags")

let p_contained_bag =
  lift2 (fun _ b -> 1, b)
    (char '1' <* (char ' '))
    (p_color <* (string "bag") ) <|>
  lift2 (fun a b -> a, b)
    (digits <* (char ' '))
    p_bags

let p_rule =
  lift2 (fun a b -> (a, b))
    (p_bags <* (string " contain "))
    ((
      (sep_by1 (string ", ") p_contained_bag) <|>
      lift (fun _ -> []) (string "no other bags")
    ) <* (char '.'))

let parse_rule r =
  CCResult.get_or_failwith (parse_string ~consume:All p_rule r)
