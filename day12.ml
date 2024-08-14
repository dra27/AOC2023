let parse lines =
  let parse line =
    let i = String.index line ' ' in
    let checks =
      let checks = String.sub line (i + 1) (String.length line - i - 1) in
      List.map int_of_string (String.split_on_char ',' checks) in
    String.sub line 0 i, checks in
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

let expand n (line, checks) =
  let line =
    let l = String.length line in
    let b = Bytes.create (n * (l + 1)) in
    for i = 1 to n do
      Bytes.unsafe_blit_string line 0 b ((i - 1) * (l + 1)) l;
      Bytes.unsafe_set b (i * (l + 1) - 1) '?'
    done;
    Bytes.unsafe_set b (Bytes.length b - 1) '.';
    Bytes.unsafe_to_string b in
  let checks = List.concat (List.init n (Fun.const checks)) in
  (line, checks)

let fits_at line i size =
  let l = String.length line in
  let j = i + size - 1 in
  if j >= l then
    false
  else
    let rec loop i =
      if i <= j && (line.[i] = '#' || line.[i] = '?') then
        i = j || loop (succ i)
      else
        false in
    loop i && (j + 1 = l || line.[j + 1] = '.' || line.[j + 1] = '?')

let arrangements n line =
  let line, checks = expand n line in
  let cache = Hashtbl.create 256 in
  let rec analyse i all_checks =
    if i = String.length line then
      if all_checks = [] then
        1
      else
        0
    else
      let key = i, all_checks in
      try Hashtbl.find cache key
      with Not_found ->
        let result =
          match all_checks with
          | check::checks ->
              if fits_at line i check then
                (if line.[i] = '?' then
                  analyse (succ i) all_checks
                else
                  0) + analyse (i + check + 1) checks
              else
                if line.[i] = '#' then
                  0
                else
                  analyse (succ i) all_checks
          | [] ->
              if String.index_from_opt line i '#' = None then
                1
              else
                0 in
        Hashtbl.add cache key result; result in
  analyse 0 checks

let solve n records =
  List.fold_left (+) 0 (List.map (arrangements n) records)

let part1 = solve 1
let test_part1 = part1 test
let solution_part1 = part1 records

let part2 = solve 5
let test_part2 = part2 test
let solution_part2 = part2 records

let () =
  Printf.printf "Day 12; Puzzle 1; test = %d\n\
                 Day 12; Puzzle 1 = %d\n\
                 Day 12; Puzzle 2; test = %d\n\
                 Day 12; Puzzle 2 = %d\n" test_part1 solution_part1
                                          test_part2 solution_part2
