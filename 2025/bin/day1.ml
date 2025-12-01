type rotation =
  | Left
  | Right

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let parse_rotation = function
  | 'L' -> Left
  | 'R' -> Right
  | _ -> failwith "Unknown rotation"
;;

let rotation_to_str = function
  | Left -> "L"
  | Right -> "R"
;;

let parse inp =
  let first_char = inp.[0] in
  let rest = String.sub inp 1 (String.length inp - 1) in
  let rest =
    try int_of_string rest with
    | Failure _ ->
      Printf.eprintf "int_of_string failed on input: %S\n" rest;
      raise (Failure ("int_of_string: bad input " ^ rest))
  in
  parse_rotation first_char, rest
;;

let do_move start rotation amount =
  let n = 100 in
  let next =
    match rotation with
    | Left -> start - amount
    | Right -> start + amount
  in
  if next >= n || next <= 0
  then (
    let dist_to_zero =
      match rotation with
      | Left -> start
      | Right -> 100 - start
    in
    let num_times = 1 + ((amount - dist_to_zero) / n) in
    (* Prevent double counting *)
    let num_times = if start = 0 && rotation = Left then num_times - 1 else num_times in
    let res = next mod n in
    if res < 0 then res + n, num_times else res, num_times)
  else next, 0
;;

let part1 lst =
  let rec aux acc start lst =
    match lst with
    | [] -> acc
    | (rotation, amount) :: xs ->
      let next, _ = do_move start rotation amount in
      let count = Bool.to_int (next = 0) in
      (* Printf.printf "%s: %d -> %d: %d\n" (rotation_to_str rotation) start next count; *)
      aux (count + acc) next xs
  in
  aux 0 50 lst
;;

let part2 lst =
  let rec aux acc start lst =
    match lst with
    | [] -> acc
    | (rotation, amount) :: xs ->
      let next, count = do_move start rotation amount in
      (* Printf.printf "%s: %d -> %d: %d\n" (rotation_to_str rotation) start next count; *)
      aux (count + acc) next xs
  in
  aux 0 50 lst
;;

let () =
  let lines =
    read_whole_file "inputs/day1-sample.txt"
    |> String.split_on_char '\n'
    |> List.map parse
  in
  let count = part1 lines in
  Printf.printf "Part 1: %d\n" count;
  let count = part2 lines in
  Printf.printf "Part 2: %d\n" count
;;
