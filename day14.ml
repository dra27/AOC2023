let test = String.split_on_char '\n' (String.trim {|
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
|})

let platform =
  In_channel.with_open_text "input-14" @@ fun ic ->
    In_channel.input_lines ic

let rec part1 load row_weight stones = function
| row::rows ->
    let l = String.length row in
    let rec loop load index =
      if index = l then
        load
      else
        let load =
          if row.[index] = '#' then begin
            let count = stones.(index) in
            stones.(index) <- 0;
            let weight = (count * (2 * row_weight - count + 1)) / 2 in
            load + weight
          end else
            load in
        if row.[index] = 'O' then
          stones.(index) <- succ stones.(index);
        loop load (succ index) in
    part1 (loop load 0) (succ row_weight) stones rows
| [] ->
    load

let part1 platform =
  match platform with
  | row::_ ->
      let l = String.length row in
      let platform =
        List.rev ((String.make l '#')::platform) in
      part1 0 0 (Array.make l 0) platform
  | [] ->
      assert false

let test_part1 = part1 test
let solution_part1 = part1 platform

let () =
  Printf.printf "Day 14; Puzzle 1; test = %d\n\
                 Day 14; Puzzle 1 = %d\n" test_part1 solution_part1
