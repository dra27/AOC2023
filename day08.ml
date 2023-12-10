module StringMap = Map.Make(String)

type node = {
  key: string;
  mutable left: node;
  mutable right: node;
}

let rec empty = {key = ""; left = empty; right = empty}

let left {left; _} = left
let right {right; _} = right

let load_map lines =
  let f map line =
    let get_map key map =
      match StringMap.find key map with
      | entry ->
          entry, map
      | exception Not_found ->
          let entry = {key; left = empty; right = empty} in
          entry, StringMap.add key entry map in
    let left, map = get_map (String.sub line 7 3) map in
    let right, map = get_map (String.sub line 12 3) map in
    let key = String.sub line 0 3 in
    match StringMap.find key map with
    | entry ->
        entry.left <- left;
        entry.right <- right;
        map
    | exception Not_found ->
        StringMap.add key {key; left; right} map
  in
  StringMap.find "AAA" (List.fold_left f StringMap.empty lines)

let route route =
  let rec parse acc n =
    if n < 0 then
      acc
    else
      parse ((if route.[n] = 'L' then left else right)::acc) (pred n) in
  parse [] (String.length route - 1)

let walk plan entry =
  let rec walk ((count, entry) as acc) = function
  | take::route ->
      let entry = take entry in
      if entry.key = "ZZZ" then
        count
      else
        walk (succ count, entry) route
  | [] ->
      walk acc plan in
  walk (1, entry) plan

let test1 = String.split_on_char '\n'
{|AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)|}

let test2 = String.split_on_char '\n'
{|AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}

let test1_part1 = walk (route "RL") (load_map test1)
let test2_part1 = walk (route "LLR") (load_map test2)
let solution_part1 =
  In_channel.with_open_text "input-08" @@ fun ic ->
    let plan = route (input_line ic) in
    let _ = input_line ic in
    let entry = load_map (In_channel.input_lines ic) in
    walk plan entry

let () =
  Printf.printf "Day 8; Puzzle 1; test1 = %d\n\
                 Day 8; Puzzle 1; test2 = %d\n\
                 Day 8; Puzzle 1 = %d\n" test1_part1 test2_part1 solution_part1
