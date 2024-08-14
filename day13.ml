module Array = struct
  include Array

  let findi_index p a =
    let n = length a in
    let rec loop i =
      if i = n then None
      else if p i (unsafe_get a i) then Some i
      else loop (succ i) in
    loop 0
end

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

let transpose_pattern rev_rows =
  let rev_rows = Array.of_list rev_rows in
  let l = Array.length rev_rows in
  let j = l - 1 in
  let init n =
    let init i = rev_rows.(j - i).[n] in
    String.init l init in
  List.init (String.length rev_rows.(0)) init

let rec part1 total multiplier rev_rows index reflection current last = function
| ""::lines ->
    let total =
      if reflection <> 0 then
        total + multiplier * reflection
      else
        let lines = transpose_pattern rev_rows in
        assert (multiplier = 100);
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

type smudge = Neq | Eq | Smudge
let smudge_eq a b =
  let rec search acc i =
    if i < 0 then
      acc
    else
      if a.[i] = b.[i] then
        search acc (pred i)
      else if acc = Eq then
        search Smudge (pred i)
      else
        Neq in
  search Eq (String.length a - 1)

let smudged_reflection pattern row _ =
  if row = 0 then
    false
  else
    let l = Array.length pattern in
    let i, j =
      if row <= l / 2 then
        0, 2 * row - 1
      else
        2 * (row - l / 2) - (l mod 2), l - 1 in
    let rec loop i j smudge =
      let smudge =
        match smudge_eq pattern.(i) pattern.(j) with
        | Neq ->
            Neq
        | Eq ->
            smudge
        | Smudge ->
            if smudge = Smudge then
              Neq
            else
              Smudge in
      if smudge = Neq || i + 1 = j then
        smudge = Smudge
      else
        loop (succ i) (pred j) smudge in
    loop i j Eq

let rec part2 total multiplier rev_current = function
| ""::lines ->
    let this = Array.of_list rev_current in
    let total =
      match Array.findi_index (smudged_reflection this) this with
      | Some index ->
          let index = Array.length this - index in
          index * multiplier + total
      | None ->
          let lines = transpose_pattern rev_current in
          assert (multiplier = 100);
          part2 total 1 [] lines in
    if lines = [] then
      total
    else
      part2 total multiplier [] lines
| line::lines ->
    part2 total multiplier (line::rev_current) lines
| [] ->
    part2 total multiplier rev_current [""]

let part2 lines =
  part2 0 100 [] lines

let test_part1 = part1 test
let solution_part1 = part1 patterns

let test_part2 = part2 test
let solution_part2 = part2 patterns

let () =
  Printf.printf "Day 13; Puzzle 1; test = %d\n\
                 Day 13; Puzzle 1 = %d\n\
                 Day 13; Puzzle 2; test = %d\n\
                 Day 13; Puzzle 2 = %d\n" test_part1 solution_part1
                                          test_part2 solution_part2
