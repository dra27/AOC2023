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
  List.fold_left f StringMap.empty lines

let route route =
  let rec parse acc n =
    if n < 0 then
      acc
    else
      parse ((if route.[n] = 'L' then left else right)::acc) (pred n) in
  parse [] (String.length route - 1)

let walk is_exit plan entry =
  let rec walk ((count, entry) as acc) = function
  | take::route ->
      let entry = take entry in
      if is_exit entry.key then
        count, entry, route
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

let test1_part2 = String.split_on_char '\n'
{|11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)|}

let part1_start  = StringMap.find "AAA"
let part1_exit = String.equal "ZZZ"
let part2_exit key = key.[2] = 'Z'

let test1_part1, _, _ =
  walk part1_exit (route "RL") (part1_start (load_map test1))
let test2_part1, _, _ =
  walk part1_exit (route "LLR") (part1_start (load_map test2))

let map, plan =
In_channel.with_open_text "input-08" @@ fun ic ->
  let plan = route (input_line ic) in
  let _ = input_line ic in
  load_map (In_channel.input_lines ic), plan

let solution_part1, _, _ =
  walk part1_exit plan (part1_start map)

let counts =
  let starts =
    let is_start elt = (elt.key.[2] = 'A') in
    let f _ elt acc = if is_start elt then elt::acc else acc in
    StringMap.fold f map [] in
  let first_ends = List.map (walk part2_exit plan) starts in
  List.map (fun (count, finish, route) ->
    (* Check that the solution is "simple" *)
    let count2, finish2, route2 = walk part2_exit plan finish in
    assert (route = [] && route2 = [] && finish == finish2 && count = count2);
    count) first_ends

(* At this point, the puzzle appears to have been set such that all the the
   counts have two prime factors and all the answers have a factor in common *)

let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a mod b)

let solution_part2 =
  let gcd =
    match counts with
    | count1::count2::_ ->
      gcd count1 count2
    | _ -> assert false in
  List.fold_left (fun acc count -> acc * (count / gcd) ) gcd counts

let () =
  Printf.printf "Day 8; Puzzle 1; test1 = %d\n\
                 Day 8; Puzzle 1; test2 = %d\n\
                 Day 8; Puzzle 1 = %d\n\
                 Day 8; Puzzle 2 = %d\n" test1_part1 test2_part1 solution_part1
                                         solution_part2
