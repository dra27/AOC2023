type universe = string list

let print_universe fmt (universe : universe) =
  let print_line line =
    Format.pp_print_string fmt line;
    Format.pp_print_newline fmt () in
  Format.pp_print_newline fmt ();
  List.iter print_line universe

let test : universe = String.split_on_char '\n' (String.trim {|
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
|})

let universe : universe =
  In_channel.with_open_text "input-11" @@ fun ic ->
    In_channel.input_lines ic

let[@tail_mod_cons] rec expand_vertically columns = function
| line::lines ->
    let scan (b, i) c =
      if c = '.' then
        (b, succ i)
      else begin
        columns.(i) <- false;
        (false, succ i)
      end in
    if fst (String.fold_left scan (true, 0) line) then
      line::line::(expand_vertically columns lines)
    else
      line::(expand_vertically columns lines)
| [] -> []

let[@tail_mod_cons] rec compute_expansion columns start i =
  if i = Array.length columns then
    if start >= 0 then
      [(start, i - start)]
    else
      [(i, 0)]
  else
    if columns.(i) then
      compute_expansion columns (if start >= 0 then start else i) (succ i)
    else if start >= 0 then
      (start, i - start)::(compute_expansion columns (-1) (succ i))
    else
      compute_expansion columns start (succ i)

let compute_expansion columns = compute_expansion columns (-1) 0

let expand_horizontally expansion line =
  let expand_horizontally (last_index, last_length) ((index, length) as next) =
    let segment =
      let init i =
        if i < index then
          line.[i + last_index]
        else
          '.'
      in
      String.init (index - last_index + length) init in
    next, segment in
  let _, segments = List.fold_left_map expand_horizontally (0, 0) expansion in
  String.concat "" segments

let expand lines : universe =
  match lines with
  | line::_ ->
      let length = String.length line in
      let columns = Array.make length true in
      let lines = expand_vertically columns lines in
      let expansions = compute_expansion columns in
      List.map (expand_horizontally expansions) lines
  | [] -> assert false

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

(* Adapted from "A Rasterizing Algorithm for Drawing Curves" Alois Zingl *)
let shortest_path (x1, y1) (x2, y2) =
  let dx = abs (x2 - x1) in
  let sx = if x1 < x2 then 1 else -1 in
  let dy = -abs (y2 - y1) in
  let sy = if y1 < y2 then 1 else -1 in
  let[@tail_mod_cons] rec loop last_x last_y err x y =
    if last_x = x2 && last_y = y2 then
      []
    else
      let e2 = 2 * err in
      let err, x' =
        if e2 >= dy then
          err + dy, x + sx
        else
          err, x in
      let err, y' =
        if e2 <= dx then
          err + dx, y + sy
        else
          err, y in
      if x = last_x || y = last_y then
        (x, y)::(loop x y err x' y')
      else
        (x, last_y)::(x, y)::(loop x y err x' y') in
  loop x1 y1 (dx + dy) x1 y1

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

let part1 universe =
  let f acc ((x1, y1 as galaxy1), (x2, y2 as galaxy2)) =
    let path = shortest_path galaxy1 galaxy2 in
    List.length path - 1 + acc in
  List.fold_left f 0 (all_pairs (find_galaxies (expand universe)))

let () =
  Printf.printf "Day 11; Puzzle 1; test = %d\n\
                 Day 11; Puzzle 1 = %d\n" (part1 test) (part1 universe)
