let[@tail_mod_cons] rec evaluate_map chains map =
  match chains, map with
  | (((current::_) as entry)::remaining_chains),
    ((source, destination, length)::next_map) ->
      if current < source then
        let this = current::entry in
        this :: evaluate_map remaining_chains map
      else if current < source + length then
        let this = (current - source + destination)::entry in
        this :: evaluate_map remaining_chains map
      else
        evaluate_map chains next_map
  | (((current::_) as entry)::remaining_chains), [] ->
      let this = current::entry in
      this :: evaluate_map remaining_chains []
  | [], _ ->
      []
  | _, _ ->
      failwith "Sir, it's quite possible this algorithm is not entirely stable"

let print_chains chains =
  let print_chain chain =
    print_endline (String.concat " -> " (List.rev_map string_of_int chain)) in
  List.iter print_chain chains

let rec process_maps ~test chains acc = function
| "" :: _ :: lines ->
    let chains = evaluate_map chains (List.sort Stdlib.compare acc) in
    process_maps ~test (List.sort Stdlib.compare chains) [] lines
| line::lines -> begin
    match String.split_on_char ' ' line with
    | [destination; source; length] ->
        let this =
          int_of_string source,
          int_of_string destination,
          int_of_string length in
        process_maps ~test chains (this::acc) lines
    | _ ->
        failwith "There do be something wraang with this 'ere almanac"
  end
| [] ->
    let chains = evaluate_map chains (List.sort Stdlib.compare acc) in
    if test then
      print_chains chains;
    List.hd (List.hd chains)

let process ?(test=false) lines =
  match lines with
  | seeds :: "" :: _ :: lines -> begin
      match String.split_on_char ' ' seeds with
      | "seeds:" :: seeds ->
          let chains =
            let f s = [int_of_string s] in
            List.sort Stdlib.compare (List.rev_map f seeds) in
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

let test_part1 =
  process (*~test:true*) (String.split_on_char '\n' test_input)
let solution_part1 =
  In_channel.with_open_text "input-05" @@ fun ic ->
    process (In_channel.input_lines ic)

let () =
  Printf.printf "Day 5; Puzzle 1; test = %d\n\
                 Day 5; Puzzle 1 = %d\n" test_part1 solution_part1
