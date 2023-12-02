{
type direction = Forward | Backward | DigitOnly
}

rule scan dir = parse
| [ '0'-'9' ] as c
    {Char.code c - 48}

| "zero"  {if dir = Forward then 0 else scan dir lexbuf}
| "one"   {if dir = Forward then 1 else scan dir lexbuf}
| "two"   {if dir = Forward then 2 else scan dir lexbuf}
| "three" {if dir = Forward then 3 else scan dir lexbuf}
| "four"  {if dir = Forward then 4 else scan dir lexbuf}
| "five"  {if dir = Forward then 5 else scan dir lexbuf}
| "six"   {if dir = Forward then 6 else scan dir lexbuf}
| "seven" {if dir = Forward then 7 else scan dir lexbuf}
| "eight" {if dir = Forward then 8 else scan dir lexbuf}
| "nine"  {if dir = Forward then 9 else scan dir lexbuf}

| "orez"  {if dir = Backward then 0 else scan dir lexbuf}
| "eno"   {if dir = Backward then 1 else scan dir lexbuf}
| "owt"   {if dir = Backward then 2 else scan dir lexbuf}
| "eerht" {if dir = Backward then 3 else scan dir lexbuf}
| "ruof"  {if dir = Backward then 4 else scan dir lexbuf}
| "evif"  {if dir = Backward then 5 else scan dir lexbuf}
| "xis"   {if dir = Backward then 6 else scan dir lexbuf}
| "neves" {if dir = Backward then 7 else scan dir lexbuf}
| "thgie" {if dir = Backward then 8 else scan dir lexbuf}
| "enin"  {if dir = Backward then 9 else scan dir lexbuf}

| _
    {scan dir lexbuf}

{
module String = struct
  include String

  let rev s =
    let l = String.length s - 1 in
    String.init (l + 1) (fun i -> s.[l - i])
end

let code1 s =
  10 * scan DigitOnly (Lexing.from_string s)
     + scan DigitOnly (Lexing.from_string (String.rev s))

let code2 s =
  10 * scan Forward (Lexing.from_string s)
     + scan Backward (Lexing.from_string (String.rev s))

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
}
