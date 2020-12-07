type pair = { key : string; value : string }
let pair key value = { key; value }

let count_keys entry =
  CCList.fold_left
    (fun acc -> function | {key="cid"; value=_} -> acc | _ -> acc + 1)
    0
    entry

let solve entries =
  CCList.map count_keys entries
  |> CCList.count (fun e -> e == 7)

let parse entry =
  entry
  |> Str.split (Str.regexp "[ \n]")
  |> CCList.map (fun s ->
      let i = CCString.index s ':' in
      pair
        (CCString.sub s 0 i)
        (CCString.sub s (i+1) ((CCString.length s) - (i+1)))
    )

let test_input =
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"

let run input =
  input
  |> CCString.split ~by:"\n\n"
  |> CCList.map parse
  |> solve

let () =
  assert ((run test_input) == 2);
  let input = Helpers.input 4 in
  print_int (run (Lwt_main.run input))
