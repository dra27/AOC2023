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

let solution =
  In_channel.with_open_text "input-01" @@ fun ic ->
    In_channel.input_lines ic
    |> List.fold_left (fun a l -> a + code l) 0

let () =
  Printf.printf "Day 1; Puzzle 1 = %d\n%!" solution
