module String = struct
  include String

  let rec findaux f s l next i =
    if i = l || i < 0 then
      raise Not_found
    else
      let c = s.[i] in
      if f c then
        c
      else
        findaux f s l next (next i)

  let find s f =
    findaux f s (String.length s) succ 0

  let rfind s f =
    let l = String.length s in
    findaux f s l pred (pred l)
end

let is_digit = function
| '0'..'9' -> true
| _ -> false

let code s =
  int_of_string (Printf.sprintf "%c%c" (String.find s is_digit)
                                       (String.rfind s is_digit))

let test_input_part1 = ["1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet"]

let input =
  In_channel.with_open_text "input-01" In_channel.input_lines

let test_part1 =
  List.fold_left (fun a l -> a + code l) 0 test_input_part1

let solution_part1 =
  List.fold_left (fun a l -> a + code l) 0 input

let () =
  Printf.printf "Day 1; Puzzle 1; test = %d\n%!" test_part1;
  Printf.printf "Day 1; Puzzle 1 = %d\n%!" solution_part1
