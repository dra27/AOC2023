{
module IntSet = Set.Make(Int)
}

rule process f acc = parse
| "Card" ' '+ (['0'-'9']+ as number) ':' ' '+
    {let winning = parse_list IntSet.empty lexbuf in
     let numbers = parse_list IntSet.empty lexbuf in
     let score = IntSet.cardinal (IntSet.inter winning numbers) in
     process f (f acc (int_of_string number) score) lexbuf}

| eof
    {acc}

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
let part1 lexbuf =
  process (fun acc _ score -> acc + 1 lsl (score - 1)) 0 lexbuf

let part2 lexbuf =
  let count, scores =
    match process (fun acc index score -> (index, score)::acc) [] lexbuf with
    | ((count, _)::_) as scores -> count, List.rev scores
    | [] -> failwith "These are not the scratchcards you're looking for" in
  let counts = Array.make count 1 in
  let rec process (index, score) =
    let this = counts.(index - 1) in
    for i = index to index + score - 1 do
      counts.(i) <- counts.(i) + this
    done
  in
  List.iter process scores;
  Array.fold_left (+) 0 counts

let test_input_part1 =
{|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
|}

let test_part1 = part1 (Lexing.from_string test_input_part1)
let solution part =
  In_channel.with_open_text "input-04" @@ fun ic ->
    part (Lexing.from_channel ic)
let solution_part1 = solution part1
let test_part2 = part2 (Lexing.from_string test_input_part1)
let solution_part2 = solution part2

let () =
  Printf.printf "Day 4; Puzzle 1; test = %d\n\
                 Day 4; Puzzle 1 = %d\n\
                 Day 4; Puzzle 2; test = %d\n\
                 Day 4; Puzzle 2 = %d\n" test_part1 solution_part1
                                         test_part2 solution_part2
}
