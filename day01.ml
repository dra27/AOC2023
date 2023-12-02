module String = struct
  include String

  let rec findaux f s l next i =
    if i = l || i < 0 then
      raise Not_found
    else
      match f s i with
      | Some r -> r
      | None ->
          findaux f s l next (next i)

  let first_left s f =
    findaux f s (String.length s) succ 0

  let first_right s f =
    let l = String.length s in
    findaux f s l pred (pred l)
end

let is_digit = function
| '0'..'9' -> true
| _ -> false

let is_digit1 s i =
  let c = s.[i] in
  if is_digit c then
    Some (Char.code c - 48)
  else
    None

let digits = [
  "zero"; "one"; "two"; "three"; "four";
  "five"; "six"; "seven"; "eight"; "nine"
]

let is_digit2 s _ i =
  let c = s.[i] in
  match c with
  | '0'..'9' -> Some (Char.code c - 48)
  | _ ->
      let f w = String.sub s i (String.length w) = w in
      List.find_index f digits

let code1 s =
  10 * String.first_left s is_digit1 + String.first_right s is_digit1

let code2 s =
  let is_digit2 = is_digit2 (s ^ "zzzz") in
  10 * String.first_left s is_digit2 + String.first_right s is_digit2

let test_input_part1 = ["1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet"]

let test_input_part2 = [
  "two1nine";
  "eightwothree";
  "abcone2threexyz";
  "xtwone3four";
  "4nineeightseven2";
  "zoneight234";
  "7pqrstsixteen"
]

let input =
  In_channel.with_open_text "input-01" In_channel.input_lines

let fold_over f l =
  List.fold_left (fun a l -> a + f l) 0 l

let test_part1 = fold_over code1 test_input_part1
let solution_part1 = fold_over code1 input
let test_part2 = fold_over code2 test_input_part2
let solution_part2 = fold_over code2 input

let () =
  Printf.printf "Day 1; Puzzle 1; test = %d\n\
                 Day 1; Puzzle 1 = %d\n\
                 Day 1; Puzzle 2; test = %d\n\
                 Day 1; Puzzle 2 = %d\n" test_part1 solution_part1
                                         test_part2 solution_part2
