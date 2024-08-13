let parse lines =
  let parse line =
    let i = String.index line ' ' in
    let checks =
      let checks = String.sub line (i + 1) (String.length line - i - 1) in
      List.map int_of_string (String.split_on_char ',' checks) in
    String.sub line 0 i ^ ".", checks in
  List.map parse lines

let test = parse (String.split_on_char '\n' (String.trim {|
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
|}))

let records =
  In_channel.with_open_text "input-12" @@ fun ic ->
    parse (In_channel.input_lines ic)

let arrangements (line, checks) =
  let rec analyse i current_bad all_checks =
    if i = String.length line then
      if all_checks = [] then
        1
      else
        0
    else
      let c = line.[i] in
      match all_checks with
      | check::checks ->
          if c = '#' then
            if current_bad < check then
              analyse (succ i) (succ current_bad) all_checks
            else
              0
          else if c = '.' then
            if current_bad > 0 then
              if current_bad = check then
                analyse (succ i) 0 checks
              else
                0
            else
              analyse (succ i) 0 all_checks
            else if c = '?' then
              if current_bad = check then
                analyse (succ i) 0 checks
              else if current_bad = 0 then
                analyse (succ i) 1 all_checks +
                analyse (succ i) 0 all_checks
              else
                analyse (succ i) (succ current_bad) all_checks
            else
              assert false
      | [] ->
          if c = '#' then
            0
          else begin
            assert ((c = '.' || c = '?') && current_bad = 0);
            analyse (succ i) 0 []
          end in
  analyse 0 0 checks

let part1 records = List.fold_left (+) 0 (List.map arrangements records)

let test_part1 = part1 test
let solution_part1 = part1 records

let () =
  Printf.printf "Day 12; Puzzle 1; test = %d\n\
                 Day 12; Puzzle 1 = %d\n" test_part1 solution_part1
