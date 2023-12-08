let rank =
  [| 0; 1; 2; 3; 4; 5; 6; 7;  0; 0; 0; 0; 0; 0;  0; 12; 0;
     0; 0; 0; 0; 0; 0; 0; 9; 11; 0; 0; 0; 0; 0; 10;  0; 0; 8 |]

type hand_type =
  High | Pair | TwoPair | Three | House | Four | Five

let hand_type hand =
  let counts = Array.make 13 0 in
  let scan_hand c =
    let i = rank.(Char.code c - 50) in
    counts.(i) <- succ counts.(i) in
  String.iter scan_hand hand;
  let counts =
    Array.fold_left (fun acc c -> if c <> 0 then c::acc else acc) [] counts in
  match List.sort (fun x y -> - Int.compare x y) counts with
  | 5 :: _ -> Five
  | 4 :: _ -> Four
  | [3; 2] -> House
  | 3 :: _ -> Three
  | 2 :: 2 :: _ -> TwoPair
  | 2 :: _ -> Pair
  | _ -> High

let compare_hands l r =
  let rec loop i =
    let l = l.[i] in
    let r = r.[i] in
    if l = r then
      loop (succ i)
    else
      rank.(Char.code l - 50) - rank.(Char.code r - 50) in
  loop 0

let process input =
  let f line =
    let hand = String.sub line 0 5 in
    let bid =
      int_of_string (String.sub line 6 (String.length line - 6)) in
    (hand_type hand, line, bid) in
  let compare (l, left_hand, _) (r, right_hand, _) =
    if l = r then
      compare_hands left_hand right_hand
    else
      Stdlib.compare l r in
  let input = List.sort compare (List.map f input) in
  let f (rank, total) (_, _, bid) = (succ rank, rank * bid + total) in
  snd (List.fold_left f (1, 0) input)

let test_input =
{|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

let test_input = String.split_on_char '\n' test_input
let test_part1 = process test_input
let solution_part1 =
  In_channel.with_open_text "input-07" @@ fun ic ->
    process (In_channel.input_lines ic)

let () =
  Printf.printf "Day 5; Puzzle 1; test = %d\n\
                 Day 5; Puzzle 1 = %d\n" test_part1 solution_part1
