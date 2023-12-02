rule game acc red green blue = parse
| "Game " ([^ ':']+ as number) ": "
    {let r, g, b = parse_game 0 0 0 lexbuf in
     let acc =
       if r <= red && g <= green && b <= blue then
         acc + int_of_string number
       else
         acc in
     game acc red green blue lexbuf}

| eof
    {acc}

and parse_game red green blue = parse
| ([^ ' ']+ as count) " red"
    {parse_game (max red (int_of_string count)) green blue lexbuf}
| ([^ ' ']+ as count) " green"
    {parse_game red (max green (int_of_string count)) blue lexbuf}
| ([^ ' ']+ as count) " blue"
    {parse_game red green (max blue (int_of_string count)) lexbuf}

| [',' ';'] ' '
    {parse_game red green blue lexbuf}

| '\r'? '\n'
    {red, green, blue}

{
let test_input_part1 =
{|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|}

let test_part1 = game 0 12 13 14 (Lexing.from_string test_input_part1)
let solution_part1 =
  In_channel.with_open_text "input-02" @@ fun ic ->
    game 0 12 13 14 (Lexing.from_channel ic)

let () =
  Printf.printf "Day 2; Puzzle 1; test = %d\n\
                 Day 2; Puzzle 1 = %d\n" test_part1 solution_part1
}
