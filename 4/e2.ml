open Parser

type pair = { key : string; value : string }
let pair key value = { key; value }

let check_year pair field l u =
  if
    match pair with
    | {key = k; value = v} when k = field ->
      let y = int_of_string v in y >= l && y <= u
    | _ -> true
  then CCResult.return pair
  else CCResult.fail field

let check_byr pair =
  check_year pair "byr" 1920 2002

let check_iyr pair =
  check_year pair "iyr" 2010 2020

let check_eyr pair =
  check_year pair "eyr" 2020 2030

let check_hgt pair = 
  if
    match pair with
    | {key = "hgt"; value = v} ->
      begin match (parse_hgt v) with
        | Some(In i) -> i >= 59 && i <= 76
        | Some(Cm i) -> i >= 150 && i <= 193
        | None -> false
      end
    | _ -> true
  then CCResult.return pair
  else CCResult.fail "hgt"

let check_hcl pair = 
  if
    match pair with
    | {key = "hcl"; value = v} ->
      begin match (parse_hcl v) with
        | Some _ -> true
        | None -> false
      end
    | _ -> true
  then CCResult.return pair
  else CCResult.fail "hcl"

let check_ecl pair = 
  if
    match pair with
    | {key = "ecl"; value = v} ->
      begin match v with
        | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
        | _ -> false
      end
    | _ -> true
  then CCResult.return pair
  else CCResult.fail "ecl"

let check_pid pair = 
  if
    match pair with
    | {key = "pid"; value = v} ->
      begin match (parse_pid v) with
        | Some _ -> true
        | _ -> false
      end
    | _ -> true
  then CCResult.return pair
  else CCResult.fail "pid"


let check_pair pair =
  let open CCResult in
  pair
  |> check_byr
  >>= check_iyr
  >>= check_eyr
  >>= check_hgt
  >>= check_hcl
  >>= check_ecl
  >>= check_pid

let count_keys entry =
  CCList.fold_left
    (fun acc -> function | {key="cid"; value=_} -> acc | _ -> acc + 1)
    0
    entry

let check_entry entry =
  let open CCResult in
  map_l check_pair entry >>= (fun _ ->
  if (count_keys entry) == 7
  then return entry
  else fail "bad count")

let solve entries =
  CCList.map check_entry entries
  |> CCList.count CCResult.is_ok

let parse entry =
  entry
  |> Str.split (Str.regexp "[ \n]")
  |> CCList.map (fun s ->
      let i = CCString.index s ':' in
      pair
        (CCString.sub s 0 i)
        (CCString.sub s (i+1) ((CCString.length s) - (i+1)))
    )

let all_ok_input =
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"

let all_pok_input =
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"

let run input =
  input
  |> CCString.split ~by:"\n\n"
  |> CCList.map parse
  |> solve

let () =
  assert ((run all_pok_input) == 0);
  assert ((run all_ok_input) == 4);
  let input = Helpers.input 4 in
  print_int (run (Lwt_main.run input))

