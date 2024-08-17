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

type cell = Empty | Stone | Block

let init_platform platform =
  match platform with
  | first::_ ->
      let rows = List.length platform in
      let columns = String.length first in
      let result = Array.init rows (fun row -> Array.make columns Empty) in
      let apply row line =
        let this = result.(row) in
        let apply i c =
          if c = 'O' then
            this.(i) <- Stone
          else if c = '#' then
            this.(i) <- Block
          else
            assert (c = '.') in
        String.iteri apply line;
        succ row in
      ignore (List.fold_left apply 0 platform);
      result
  | [] ->
      assert false

let tilt get set bounds platform =
  let i_from, i_to, j_length =
    bounds (Array.length platform) (Array.length platform.(0)) in
  let stones = Array.make j_length 0 in
  let i_step = if i_from > i_to then -1 else 1 in
  let rec loop i j =
    if j < j_length then begin
      let cell = get platform i j in
      if cell = Stone then begin
        stones.(j) <- succ stones.(j);
        set platform i j Empty
      end;
      if (cell = Block || i = i_to) && stones.(j) > 0 then begin
        let i_step = -i_step in
        let k_to =
          (i + (1 + stones.(j) - (if cell = Block then 0 else 1)) * i_step) in
        let rec move k =
          if k <> k_to then begin
            assert (get platform k j <> Block);
            set platform k j Stone;
            move (k + i_step)
          end in
        move (if cell = Block then i + i_step else i);
        stones.(j) <- 0
      end;
      loop i (succ j)
    end else if i <> i_to then
      loop (i + i_step) 0 in
  loop i_from 0

let tilt_ns =
  tilt (fun a i j -> a.(i).(j)) (fun a i j c -> a.(i).(j) <- c)
let tilt_ew =
  tilt (fun a i j -> a.(j).(i)) (fun a i j c -> a.(j).(i) <- c)

let tilt_north = tilt_ns (fun i j -> i - 1, 0, j)
let tilt_south = tilt_ns (fun i j -> 0, i - 1, j)
let tilt_east = tilt_ew (fun i j -> 0, j - 1, i)
let tilt_west = tilt_ew (fun i j -> j - 1, 0, i)

let string_of_platform platform =
  let string_of_row j =
    let row = platform.(j) in
    let char_of_cell i =
      match row.(i) with
      | Empty -> '.'
      | Block -> '#'
      | Stone -> 'O' in
    String.init (Array.length row) char_of_cell in
  String.concat "\n" (List.init (Array.length platform) string_of_row)

let load platform =
  let sum_rows (load, total) row =
    let total =
      let sum_columns total = function
      | Stone -> total + load
      | _ -> total in
      Array.fold_left sum_columns total row in
    (pred load, total) in
  let _, load =
    Array.fold_left sum_rows (Array.length platform, 0) platform in
  load

let part1 platform =
  let platform = init_platform platform in
  tilt_north platform;
  load platform

let part2 platform =
  let cache = Hashtbl.create 8192 in
  let platform = init_platform platform in
  let rec loop n =
    let key = string_of_platform platform in
    if Hashtbl.mem cache key then
      n
    else begin
      Hashtbl.add cache key (n, load platform);
      tilt_north platform;
      tilt_west platform;
      tilt_south platform;
      tilt_east platform;
      loop (succ n)
    end in
  let prefix = loop 0 in
  let cycle, _ = Hashtbl.find cache (string_of_platform platform) in
  let phase = (1_000_000_000 - cycle + 1) mod (prefix - cycle) + cycle - 1 in
  let find _ (n, load) acc = if n = phase then load else acc in
  Hashtbl.fold find cache 0

let test_part1 = part1 test
let solution_part1 = part1 platform

let test_part2 = part2 test
let solution_part2 = part2 platform

let () =
  Printf.printf "Day 14; Puzzle 1; test = %d\n\
                 Day 14; Puzzle 1 = %d\n\
                 Day 14; Puzzle 2; test = %d\n\
                 Day 14; Puzzle 2 = %d\n" test_part1 solution_part1
                                          test_part2 solution_part2
