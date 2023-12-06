let[@tail_mod_cons] rec evaluate_map chains map =
  match chains, map with
  | (((((current_start, current_end) as current)::chain) as entry)
      :: remaining_chains),
    ((source_start, source_end, destination)::next_map) ->
      if current_end < source_start then
        let this = current::entry in
        this :: evaluate_map remaining_chains map
      else if source_end < current_start then
        evaluate_map chains next_map
      else if current_start < source_start then
        let chains =
          let left_chain (x, _) = x, x + source_start - 1 - current_start in
          let right_chain (_, y) = y - (current_end - source_start), y in
          ((current_start, source_start - 1)::(List.map left_chain chain)) ::
          ((source_start, current_end)::(List.map right_chain chain)) ::
          remaining_chains in
        evaluate_map chains map
      else if current_end <= source_end then
        let this =
          (current_start - source_start + destination,
           current_end - source_start + destination) :: entry in
        this :: evaluate_map remaining_chains map
      else
        let chains =
          let left_chain (x, _) = x, x + source_end - current_start in
          let right_chain (_, y) = y - (current_end - (source_end + 1)), y in
          ((current_start, source_end)::(List.map left_chain chain)) ::
          ((source_end + 1, current_end)::(List.map right_chain chain)) ::
          remaining_chains in
        evaluate_map chains map
  | (((current::_) as entry)::remaining_chains), [] ->
      let this = current::entry in
      this :: evaluate_map remaining_chains []
  | [], _ ->
      []
  | _, _ ->
      failwith "Sir, it's quite possible this algorithm is not entirely stable"

let print_chains chains =
  let print_chain chain =
    let f (x, y) =
      if x = y then string_of_int x else Printf.sprintf "%d-%d" x y
    in
    print_endline (String.concat " -> " (List.rev_map f chain)) in
  List.iter print_chain chains

let rec process_maps ~test chains acc = function
| "" :: _ :: lines ->
    let chains = evaluate_map chains (List.sort Stdlib.compare acc) in
    process_maps ~test (List.sort Stdlib.compare chains) [] lines
| line::lines -> begin
    match String.split_on_char ' ' line with
    | [destination; source; length] ->
        let this =
          let source = int_of_string source in
          source,
          int_of_string length + source - 1,
          int_of_string destination in
        process_maps ~test chains (this::acc) lines
    | _ ->
        failwith "There do be something wraang with this 'ere almanac"
  end
| [] ->
    let chains = evaluate_map chains (List.sort Stdlib.compare acc) in
    if test then
      print_chains chains;
    fst (List.hd (List.hd (List.sort Stdlib.compare chains)))

let part1 =
  let f x =
    let x = int_of_string x in
    [(x, x)]
  in
  List.map f

let[@tail_mod_cons] rec part2 = function
| start :: length :: seeds ->
    let start = int_of_string start in
    let this = start, int_of_string length + start - 1 in
    [this] :: part2 seeds
| [] ->
    []
| [_] ->
    failwith "Why is there always one piece left over?"

let process ?(test=false) parse_seeds lines =
  match lines with
  | seeds :: "" :: _ :: lines -> begin
      match String.split_on_char ' ' seeds with
      | "seeds:" :: seeds ->
          let chains = List.sort Stdlib.compare (parse_seeds seeds) in
          process_maps ~test chains [] lines
      | _ ->
          failwith "These are not the seeds you're looking for"
    end
  | _ ->
      failwith "This is not the almanac you're looking for"

let test_input =
{|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|}

let test_input = String.split_on_char '\n' test_input
let test_part1 = process ~test:true part1 test_input
let solution part =
  In_channel.with_open_text "input-05" @@ fun ic ->
    process part (In_channel.input_lines ic)
let solution_part1 = solution part1
let test_part2 = process ~test:true part2 test_input
let solution_part2 = solution part2

let () =
  Printf.printf "Day 5; Puzzle 1; test = %d\n\
                 Day 5; Puzzle 1 = %d\n\
                 Day 5; Puzzle 1; test = %d\n\
                 Day 5; Puzzle 2 = %d\n" test_part1 solution_part1
                                         test_part2 solution_part2
