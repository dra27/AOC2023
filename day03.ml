module Array = struct
  include Array

  let foldi_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r i (unsafe_get a i)
    done;
    !r
end

module String = struct
  include String

  let foldi_right f a x =
    let r = ref x in
    for i = length a - 1 downto 0 do
      r := f i (unsafe_get a i) !r
    done;
    !r
end

let parse input =
  match input with
  | l::_ ->
      let x = String.length l in
      let guard = String.make (x + 2) '.' in
      let[@tail_mod_cons] rec loop = function
      | l::ls ->
          if String.length l <> x then
            failwith "Schematic is not uniform"
          else
            ("." ^ l ^ ".") :: loop ls
      | [] ->
          [guard] in
      Array.of_list (guard :: loop input)
  | [] ->
      failwith "These are not the parts you're looking for"

let test_input_part1 = String.split_on_char '\n'
{|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}

(* Should be 3763 (drops the 598; keeps the 755) for part 1
   Should be 463305 (drops the 755/598 gear; adds 592/755) for part 2 *)
let test_input_part1_dra27 = String.split_on_char '\n'
{|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
.....*755.
...$......
.664.598..|}

let process_input analyse_cell complete_number initial acc input =
  let scan_line acc y s =
    if y = 0 || y = Array.length input - 1 then
      acc
    else
      let f x c (analysis, current, multiplier, acc) =
        match c with
        | '0'..'9' ->
            let current = current + (Char.code c - 48) * multiplier in
            let multiplier = multiplier * 10 in
            let analysis = analyse_cell analysis input x y in
            analysis, current, multiplier, acc
        | _ ->
            let analysis, acc =
              complete_number analysis current acc input x y in
            analysis, 0, 1, acc in
      let _, _, _, acc = String.foldi_right f s (initial, 0, 1, acc) in
      acc in
  Array.foldi_left scan_line acc input

let is_symbol = function
| '0'..'9'
| '.' -> false
| _ -> true

let touches_above_or_below acc input x y =
  acc ||
  is_symbol input.(y + 1).[x] ||
  is_symbol input.(y - 1).[x] ||
  is_symbol input.(y).[x]

let part1 =
  let process_number take current acc input x y =
    let next_take = touches_above_or_below false input x y in
    let acc = if take || next_take then current + acc else acc in
    next_take, acc
  in
  process_input touches_above_or_below process_number false 0

let is_digit = function
| '0'..'9' -> true
| _ -> false

let is_gear input x y =
  if input.(y).[x] = '*' then
    let count row =
      if is_digit row.[x] then
        1
      else
        Bool.to_int (is_digit row.[x - 1]) +
        Bool.to_int (is_digit row.[x + 1]) in
    count input.(y - 1) + count input.(y) + count input.(y + 1) = 2
  else
    false

let gears_of gears input x y =
  let f y l = if is_gear input x y then (x, y)::l else l in
  f (y - 1) (f (y + 1) (f y gears))

let part2 input =
  let process_number gears current acc input x y =
    let next_gears = gears_of [] input x y in
    let acc =
      if current > 0 then
        (* Gears definitely to the left of a number *)
        (List.map (fun gear -> (gear, current)) (next_gears @ gears)) @ acc
      else
        (* Gears possibly to the right of a number *)
        acc in
    next_gears, acc in
  let rec process acc = function
  | (g, l)::(g', r)::gears ->
      if g <> g' then
        failwith "Morirò fra strazi e scempi"
      else
        process (l * r + acc) gears
  | [] ->
      acc
  | [_] ->
      failwith "Why is there always one piece left over?"
  in
  process_input gears_of process_number [] [] input
  |> List.sort Stdlib.compare
  |> process 0

let test_input_part1 = parse test_input_part1
let test_input_part1_dra27 = parse test_input_part1_dra27
let test_part1 = part1 test_input_part1
let test_part1_dra27 = part1 test_input_part1_dra27
let input =
  parse (In_channel.with_open_text "input-03" In_channel.input_lines)
let solution_part1 = part1 input
let test_part2 = part2 test_input_part1
let test_part2_dra27 = part2 test_input_part1_dra27
let solution_part2 = part2 input

let () =
  Printf.printf
    "Day 3; Puzzle 1; test = %d\n\
     Day 3; Puzzle 1; test (dra27) = %d\n\
     Day 3; Puzzle 1 = %d\n\
     Day 3; Puzzle 2; test = %d\n\
     Day 3; Puzzle 2; test (dra27) = %d\n\
     Day 3; Puzzle 2 = %d\n" test_part1 test_part1_dra27 solution_part1
                             test_part2 test_part2_dra27 solution_part2
