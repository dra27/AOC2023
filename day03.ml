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

(* Should be 3763 (drops the 598; keeps the 755) *)
let test_input_part1_dra27 = String.split_on_char '\n'
{|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
.....!755.
...$......
.664.598..|}

let is_symbol = function
| '0'..'9'
| '.' -> false
| _ -> true

let touches_above_or_below input x y =
  let above = input.(y - 1) in
  let below = input.(y + 1) in
  is_symbol above.[x - 1] || is_symbol above.[x] || is_symbol above.[x + 1]
  || is_symbol below.[x - 1] || is_symbol below.[x] || is_symbol below.[x + 1]

let part1 input =
  let input = parse input in
  let scan_line acc y s =
    let f x c (take, current, multiplier, acc) =
      match c with
      | '0'..'9' ->
          let current = current + (Char.code c - 48) * multiplier in
          let multiplier = multiplier * 10 in
          let take = take || touches_above_or_below input x y in
          take, current, multiplier, acc
      | _ ->
          let next_take = c <> '.' in
          if take || next_take then
            next_take, 0, 1, current + acc
          else
            next_take, 0, 1, acc in
    let _, _, _, acc = String.foldi_right f s (false, 0, 1, acc) in
    acc in
  Array.foldi_left scan_line 0 input

let test_part1 = part1 test_input_part1
let test_part1_dra27 = part1 test_input_part1_dra27
let solution_part1 =
  In_channel.with_open_text "input-03" @@ fun ic ->
    part1 @@ In_channel.input_lines ic

let () =
  Printf.printf
    "Day 3; Puzzle 1; test = %d\n\
     Day 3; Puzzle 1; test (dra27) = %d\n\
     Day 3; Puzzle 1 = %d\n" test_part1 test_part1_dra27 solution_part1
