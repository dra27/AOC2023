module String = struct
  include String

  let foldi_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r i (unsafe_get a i)
    done;
    !r

  let foldi_left_map f x a =
    let r = ref x in
    let l = length a in
    let b = Bytes.create l in
    for i = 0 to l - 1 do
      let r', c = f !r i (unsafe_get a i) in
      r := r';
      Bytes.unsafe_set b i c
    done;
    !r, Bytes.unsafe_to_string b
end

let parse lines =
  let analyse (galaxies, empty_rows, row, mask) line =
    let galaxies', mask =
      let f galaxies i c =
        if c = '#' then
          ((row, i)::galaxies, c)
        else
          (galaxies, mask.[i]) in
      String.foldi_left_map f galaxies line in
    let empty_rows =
      if galaxies == galaxies' then row::empty_rows else empty_rows in
    (galaxies', empty_rows, succ row, mask) in
  match lines with
  | hd::_ ->
      let galaxies, empty_rows, _, mask =
        let mask = String.make (String.length hd) '.' in
        List.fold_left analyse ([], [], 0, mask) lines in
      let empty_columns =
        let f empty_columns i = function
        | '.' -> i::empty_columns
        | _ -> empty_columns in
        String.foldi_left f [] mask in
      (galaxies, empty_rows, empty_columns)
  | [] -> assert false

let expand n (galaxies, empty_rows, empty_columns) =
  let f ref acc this = if ref > this then n + acc else acc in
  let expand (row, column) =
    let row = List.fold_left (f row) row empty_rows in
    let column = List.fold_left (f column) column empty_columns in
    (row, column) in
  List.map expand galaxies

let[@tail_mod_cons] rec all_pairs xs x = function
| y::ys ->
    (x, y)::(all_pairs xs x ys)
| [] ->
    match xs with
    | [] -> []
    | x::xs -> all_pairs xs x xs

let all_pairs xs =
  match xs with
  | x::_ -> all_pairs xs x []
  | [] -> []

let shortest_paths galaxies =
  let f acc ((x1, y1), (x2, y2)) =
    abs (x2 - x1) + abs (y2 - y1) + acc in
  List.fold_left f 0 (all_pairs galaxies)

let find_galaxies universe =
  let find_in_row (acc, y) row =
    let find_in_column (acc, x) c =
      if c = '#' then
        ((x, y)::acc, succ x)
      else
        (acc, succ x) in
    let acc, _ = String.fold_left find_in_column (acc, 0) row in
    acc, succ y in
  fst (List.fold_left find_in_row ([], 0) universe)

let test = parse (String.split_on_char '\n' (String.trim {|
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
|}))

let universe =
  In_channel.with_open_text "input-11" @@ fun ic ->
    parse (In_channel.input_lines ic)

let () =
  Printf.printf "Day 11; Puzzle 1; test = %d\n\
                 Day 11; Puzzle 1 = %d\n\
                 Day 11; Puzzle 2; test1 = %d\n\
                 Day 11; Puzzle 2; test2 = %d\n\
                 Day 11; Puzzle 2 = %d\n" (shortest_paths (expand 1 test))
                                          (shortest_paths (expand 1 universe))
                                          (shortest_paths (expand 9 test))
                                          (shortest_paths (expand 99 test))
                                          (shortest_paths (expand 999999 universe))
