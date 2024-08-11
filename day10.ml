let square = Array.of_list (String.split_on_char '\n' (String.trim {|
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
|}))

let complex = Array.of_list (String.split_on_char '\n' (String.trim {|
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
|}))

let get_start field =
  let check row s =
    Option.map (fun column -> row, column) (String.index_opt s 'S') in
  let (row, column) as start = Option.get (Array.find_mapi check field) in
  let connections = [
    (['7'; 'F'; '|'], row - 1, column);
    (['L'; 'F'; '-'], row, column - 1);
    (['J'; '7'; '-'], row, column + 1);
    (['L'; 'J'; '|'], row + 1, column);
  ] in
  let test_connection (connections, row, column) =
    if row >= 0 &&
       column >= 0 &&
       row < Array.length field &&
       column < String.length field.(0) &&
       List.mem field.(row).[column] connections then
      Some (row, column)
    else
      None in
  match List.filter_map test_connection connections with
  | [first; second] -> ((start, first), (start, second))
  | _ -> assert false

module CharMap = Map.Make(Char)

let connections =
  let f acc (c, coords) = CharMap.add c coords acc in
  List.fold_left f CharMap.empty [
    ('|', ((-1, 0), (+1, 0)));
    ('-', ((0, -1), (0, +1)));
    ('L', ((-1, 0), (0, +1)));
    ('J', ((-1, 0), (0, -1)));
    ('7', ((+1, 0), (0, -1)));
    ('F', ((+1, 0), (0, +1)));
  ]

let step field (prev, (row, column)) =
  let ((delta1_row, delta1_column), (delta2_row, delta2_column)) =
    CharMap.find field.(row).[column] connections in
  let next1 = (row + delta1_row, column + delta1_column) in
  let next2 = (row + delta2_row, column + delta2_column) in
  if next1 = prev then
    next2
  else if next2 = prev then
    next1
  else
    assert false

let rec walk field n (((_, cur1) as walk1), ((_, cur2) as walk2)) =
  let n = succ n in
  if cur1 = cur2 then
    n
  else
    let next1 = step field walk1 in
    let next2 = step field walk2 in
    if next1 = cur2 && next2 = cur1 then
      n
    else
      walk field n ((cur1, next1), (cur2, next2))

let walk field = walk field 0 (get_start field)

let test1_part1 = walk square
let test2_part1 = walk complex
let solution_part1 =
  In_channel.with_open_text "input-10" @@ fun ic ->
    walk (Array.of_list (In_channel.input_lines ic))

let () =
  Printf.printf "Day 10; Puzzle 1; test1 = %d\n\
                 Day 10; Puzzle 1; test2 = %d\n\
                 Day 10; Puzzle 1 = %d\n" test1_part1 test2_part1
                                          solution_part1
