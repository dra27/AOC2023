module Coord = struct
  type t = int * int
  let compare : t -> t -> int = Stdlib.compare
end

module CoordSet = Set.Make(Coord)

type field = {
  field: string array;
  start: Coord.t;
  loop: CoordSet.t;
}

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

let print_field fmt {field; loop; start = (start_row, start_column)} =
  let print_column row column c =
    let c =
      if row = start_row && column = start_column then
        'S'
      else
        c in
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

type north = N
type south = S
type east = E
type west = W

type _ connection =
| N : north connection
| S : south connection
| E : east connection
| W : west connection
| X : _ connection

let parse_field lines =
  let rec scan prev row = function
  | line::lines ->
      conclude prev line lines row (String.index_opt line 'S')
  | [] ->
      assert false
  and conclude prev line lines row = function
  | None ->
      scan line (succ row) lines
  | Some column ->
      let next = match lines with
      | line::_ -> line
      | [] -> "" in
      let get_if cond s column c1 c2 c3 connection =
        if cond && (s.[column] = c1 || s.[column] = c2 || s.[column] = c3) then
          connection
        else
          X in
      let north =
        get_if (row > 0) prev column '|' 'F' '7' N in
      let south =
        get_if (lines <> []) next column '|' 'J' 'L' S in
      let east =
        get_if (column < String.length line) line (column + 1) '-' 'J' '7' E in
      let west =
        get_if (column > 0) line (column - 1) '-' 'L' 'F' W in
      let start_pipe =
        match north, south, east, west with
        | N, S, X, X -> '|'
        | N, X, E, X -> 'L'
        | N, X, X, W -> 'J'
        | X, S, E, X -> 'F'
        | X, S, X, W -> '7'
        | X, X, E, W -> '-'
        | _, _, _, _ -> assert false in
      start_pipe, (row, column), row + 1 + List.length lines in
  let start_pipe, (start_row, start_column as start), length =
    scan "" 0 lines in
  let field = Array.make length "" in
  let init index line =
    let f column c = if column = start_column then start_pipe else c in
    field.(index) <- (if index = start_row then String.mapi f line else line);
    succ index in
  ignore (List.fold_left init 0 lines);
  {field; start; loop = CoordSet.empty}

let square = parse_field (String.split_on_char '\n' (String.trim {|
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
|}))

let complex = parse_field (String.split_on_char '\n' (String.trim {|
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
|}))

let get_start {field; start = (row, column) as start} =
  let first, second =
    match field.(row).[column] with
    | '|' -> ((row - 1, column), (row + 1, column))
    | '-' -> ((row, column - 1), (row, column + 1))
    | 'J' -> ((row - 1, column), (row, column - 1))
    | '7' -> ((row + 1, column), (row, column - 1))
    | 'L' -> ((row - 1, column), (row, column + 1))
    | 'F' -> ((row + 1, column), (row, column + 1))
    | _ -> assert false in
  ((start, first), (start, second))

let step {field} (prev, (row, column)) =
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

let rec walk field loop n (((_, cur1) as walk1), ((_, cur2) as walk2)) =
  let n = succ n in
  if cur1 = cur2 then
    n, {field with loop}
  else
    let next1 = step field walk1 in
    let next2 = step field walk2 in
    if next1 = cur2 && next2 = cur1 then
      n, {field with loop}
    else
      let loop = CoordSet.add next1 (CoordSet.add next2 loop) in
      walk field loop n ((cur1, next1), (cur2, next2))

let walk field =
  let ((start, next1), (_, next2)) as start_state = get_start field in
  let loop = CoordSet.of_list [start; next1; next2] in
  walk field loop 0 start_state

let test1_part1, square = walk square
let test2_part1, complex = walk complex
let field =
  In_channel.with_open_text "input-10" @@ fun ic ->
    parse_field (In_channel.input_lines ic)
let solution_part1, field = walk field


let () =
  Printf.printf "Day 10; Puzzle 1; test1 = %d\n\
                 Day 10; Puzzle 1; test2 = %d\n\
                 Day 10; Puzzle 1 = %d\n" test1_part1 test2_part1
                                          solution_part1
