{
module IntSet = Set.Make(Int)
}

rule process total = parse
| "Card" ' '+ ['0'-'9']+ ':' ' '+
    {let winning = parse_list IntSet.empty lexbuf in
     let numbers = parse_list IntSet.empty lexbuf in
     let score = 1 lsl (IntSet.cardinal (IntSet.inter winning numbers) - 1) in
     process (total + score) lexbuf}

| eof
    {total}

and parse_list acc = parse
| (['0'-'9']+ as number) ' '*
    {let number = int_of_string number in
     if IntSet.mem number acc then
       failwith "Funny scratchcard?"
     else
       parse_list (IntSet.add number acc) lexbuf}

| '|' ' '* | '\r'? '\n'
    {acc}

{
let test_input_part1 =
{|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
|}

let test_part1 = process 0 (Lexing.from_string test_input_part1)
let solution_part1 =
  In_channel.with_open_text "input-04" @@ fun ic ->
    process 0 (Lexing.from_channel ic)

let () =
  Printf.printf "Day 4; Puzzle 1; test = %d\n\
                 Day 4; Puzzle 1 = %d\n" test_part1 solution_part1
}
