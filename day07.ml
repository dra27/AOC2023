let rank =
  [|  1;  2;  3;  4;  5;  6;  7;  8; -1; -1; -1; -1; -1; -1; -1; 13; -1;
     -1; -1; -1; -1; -1; -1; -1; 10; 12; -1; -1; -1; -1; -1; 11; -1; -1; 9 |]

let rank ~jokers c =
  if jokers && c = 'J' then
    0
  else
    rank.(Char.code c - 50)

type hand_type =
  High | Pair | TwoPair | Three | House | Four | Five

let hand_type ~jokers hand =
  let counts = Array.make 14 0 in
  let scan_hand c =
    let i = rank ~jokers c in
    counts.(i) <- succ counts.(i) in
  String.iter scan_hand hand;
  let counts' =
    Array.fold_left (fun acc c -> if c <> 0 then c::acc else acc) [] counts in
  let joker from_type to_type =
    if counts.(0) > 0 then to_type else from_type in
  match List.sort (fun x y -> - Int.compare x y) counts' with
  | [5] -> Five
  | [4; 1] -> joker Four Five
  | [3; 2] -> joker House Five
  | [3; 1; 1] -> joker Three Four
  | [2; 2; 1] ->
      if counts.(0) = 1 then
        House
      else if counts.(0) = 2 then
        Four
      else
        TwoPair
  | [2; 1; 1; 1] -> joker Pair Three
  | _ -> joker High Pair

let compare_hands ~jokers l r =
  let rec loop i =
    let l = l.[i] in
    let r = r.[i] in
    if l = r then
      loop (succ i)
    else
      rank ~jokers l - rank ~jokers r in
  loop 0

let process ?(jokers=false) input =
  let f line =
    let hand = String.sub line 0 5 in
    let bid =
      int_of_string (String.sub line 6 (String.length line - 6)) in
    (hand_type ~jokers hand, line, bid) in
  let compare (l, left_hand, _) (r, right_hand, _) =
    if l = r then
      compare_hands ~jokers left_hand right_hand
    else
      Stdlib.compare l r in
  List.sort compare (List.map f input)

let winnings hands =
  let f (rank, total) (_, _, bid) = (succ rank, rank * bid + total) in
  snd (List.fold_left f (1, 0) hands)

let test_input =
{|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

let test_input_dra27 =
{|32T3K 765
T55J5 684
KK677 28
TJKJT 220
QQQJA 483|}

let test_input = String.split_on_char '\n' test_input
let test_part1 = winnings (process test_input)
let solution ~jokers =
  In_channel.with_open_text "input-07" @@ fun ic ->
    winnings (process ~jokers (In_channel.input_lines ic))
let solution_part1 = solution ~jokers:false
let test_part2 = winnings (process ~jokers:true test_input)
let test_input_dra27 = String.split_on_char '\n' test_input_dra27
let test_part2_dra27 = winnings (process ~jokers:true test_input_dra27)
let solution_part2 = solution ~jokers:true

let () =
  Printf.printf
    "Day 7; Puzzle 1; test = %d\n\
     Day 7; Puzzle 1 = %d\n\
     Day 7; Puzzle 2; test = %d\n\
     Day 7: Puzzle 2; test (dra27) = %d\n\
     Day 7; Puzzle 2 = %d\n" test_part1 solution_part1
                             test_part2 test_part2_dra27 solution_part2
