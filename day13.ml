let test = String.split_on_char '\n' (String.trim {|
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
|})

let patterns =
  In_channel.with_open_text "input-13" @@ fun ic ->
    In_channel.input_lines ic

let rec part1 total multiplier rev_rows index reflection current last = function
| ""::lines ->
    let total =
      if reflection <> 0 then
        total + multiplier * reflection
      else
        let rev_rows = Array.of_list rev_rows in
        let l = Array.length rev_rows in
        let j = l - 1 in
        let init n =
          let init i = rev_rows.(j - i).[n] in
          String.init l init in
        assert (multiplier = 100);
        let lines = List.init (String.length rev_rows.(0)) init in
        part1 total 1 [] 0 0 [] "" lines in
    if lines = [] then
      total
    else
      part1 total multiplier [] 0 0 [] "" lines
| line::lines ->
    if line = last && reflection = 0 then
      part1 total multiplier (line::rev_rows) (succ index) index
            (List.tl rev_rows) line lines
    else if reflection <> 0 then
      match current with
      | current::rest ->
          if current = line then
            part1 total multiplier (line::rev_rows) (succ index) reflection rest
                  line lines
          else
            part1 total multiplier (line::rev_rows) (succ index) 0 [] line lines
      | [] ->
         part1 total multiplier (line::rev_rows) (succ index) reflection [] line
               lines
    else
      part1 total multiplier (line::rev_rows) (succ index) 0 [] line lines
| [] ->
    part1 total multiplier rev_rows index reflection current last [""]

let part1 lines =
  part1 0 100 [] 0 0 [] "" lines

let test_part1 = part1 test
let solution_part1 = part1 patterns

let () =
  Printf.printf "Day 13; Puzzle 1; test = %d\n\
                 Day 13; Puzzle 1 = %d\n" test_part1 solution_part1
