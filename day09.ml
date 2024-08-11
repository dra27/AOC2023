(* [diff] is [Some diff] if all the differences so far have been the same.
   [~forward] is [true] if the list being traversed is in order. *)
let rec sweep diff ~forward acc next = function
| x::(y::_ as xs) ->
    let this = if forward then y - x else x - y in
    let diff = if diff = Some this then diff else None in
    sweep diff ~forward (this::acc) next xs
| [x] ->
    begin match diff with
    | None ->
        begin match acc with
        | first::(second::_ as rest) ->
            let diff = if forward then first - second else second - first in
            let next = if forward then first + x + next else next in
            sweep (Some diff) ~forward:(not forward) [diff] next rest
        | _ -> assert false
        end
    | Some diff ->
        if forward then
          next + x + diff
        else
          next + diff
    end
| [] -> assert false

let sweep xs =
  match xs with
  | first :: (second :: _ as rest) ->
      let diff = second - first in
      sweep (Some diff) ~forward:true [diff] 0 rest
  | _ ->
      assert false

let test = [
  [ 0;  3;  6;  9; 12; 15];
  [ 1;  3;  6; 10; 15; 21];
  [10; 13; 16; 21; 30; 45];
]

let test_part1 =
  List.fold_left (fun acc row -> acc + sweep row) 0 test

let parse line =
  List.map int_of_string (String.split_on_char ' ' line)

let solution_part1 =
In_channel.with_open_text "input-09" @@ fun ic ->
  In_channel.input_lines ic
  |> List.map parse
  |> List.fold_left (fun acc row -> acc + sweep row) 0

let () =
  Printf.printf "Day 9; Puzzle 1; test = %d\n\
                 Day 9; Puzzle 1 = %d\n" test_part1 solution_part1
