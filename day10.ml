type field = string array

let square : field = Array.of_list (String.split_on_char '\n' (String.trim {|
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
|}))

let complex : field = Array.of_list (String.split_on_char '\n' (String.trim {|
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

let connections, loop_pieces, pipe_pieces =
  let f (connections, loop_pieces, pipe_pieces) (c, coords, loop, pipe) =
    CharMap.add c coords connections,
    CharMap.add c loop loop_pieces,
    CharMap.add c pipe pipe_pieces in
  let connections, loop_pieces, pipe_pieces =
    List.fold_left f (CharMap.empty, CharMap.empty, CharMap.empty) [
      ('|', ((-1, 0), (+1, 0)), "\xe2\x95\x91", "\xe2\x94\x82"); (* ║ / │ *)
      ('-', ((0, -1), (0, +1)), "\xe2\x95\x90", "\xe2\x94\x80"); (* ═ / ─ *)
      ('L', ((-1, 0), (0, +1)), "\xe2\x95\x9a", "\xe2\x94\x94"); (* ╚ / └ *)
      ('J', ((-1, 0), (0, -1)), "\xe2\x95\x9d", "\xe2\x94\x98"); (* ╝ / ┘ *)
      ('7', ((+1, 0), (0, -1)), "\xe2\x95\x97", "\xe2\x94\x90"); (* ╗ / ┐ *)
      ('F', ((+1, 0), (0, +1)), "\xe2\x95\x94", "\xe2\x94\x8c"); (* ╔ / ┌ *)
    ] in
  let wrap map c =
    try CharMap.find c map
    with Not_found -> String.make 1 c in
  connections, wrap loop_pieces, wrap pipe_pieces

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

module CoordSet = Set.Make(struct type t = int * int let compare = compare end)

type solved_field = string array * CoordSet.t

let rec walk field loop n (((_, cur1) as walk1), ((_, cur2) as walk2)) =
  let n = succ n in
  if cur1 = cur2 then
    n, (field, loop : solved_field)
  else
    let next1 = step field walk1 in
    let next2 = step field walk2 in
    if next1 = cur2 && next2 = cur1 then
      n, (field, loop : solved_field)
    else
      let loop = CoordSet.add next1 (CoordSet.add next2 loop) in
      walk field loop n ((cur1, next1), (cur2, next2))

let walk field =
  let ((start, next1), (_, next2)) as start_state = get_start field in
  let loop = CoordSet.of_list [start; next1; next2] in
  walk field loop 0 start_state

let test1_part1, loop_square = walk square
let test2_part1, loop_complex = walk complex
let field : field =
  In_channel.with_open_text "input-10" @@ fun ic ->
    Array.of_list (In_channel.input_lines ic)
let solution_part1, loop = walk field

let print_field_gen loop fmt field =
  let print_column row column c =
    let pieces =
      if CoordSet.mem (row, column) loop then
        loop_pieces
      else
        pipe_pieces in
    Format.pp_print_string fmt (pieces c) in
  let print_row row columns =
    String.iteri (print_column row) columns;
    Format.pp_print_char fmt '\n' in
  Array.iteri print_row field

let print_solved_field fmt (field, loop : solved_field) =
  print_field_gen loop fmt field

let print_field fmt (field : field) =
  print_field_gen CoordSet.empty fmt field

let () =
  Printf.printf "Day 10; Puzzle 1; test1 = %d\n\
                 Day 10; Puzzle 1; test2 = %d\n\
                 Day 10; Puzzle 1 = %d\n" test1_part1 test2_part1
                                          solution_part1
