#use "./extensions.ml";;

let up_or_down ch =
  match ch with
  | '(' -> 1
  | ')' -> -1
  | _ -> 0 (* ignore bad instructions *)
;;

let part1 () =
  File.open_in "day01.input" (fun ch ->
    Stream.of_chars ch
    |> Stream.map up_or_down
    |> Stream.fold ( + ) 0
    |> Printf.printf "part 1: ended up on floor %d\n";
  )
;;

let part2 () =
  File.open_in "day01.input" (fun ch ->
    let rec walk_to_basement floor steps stream =
      if floor = -1 then
        Printf.printf "part 2: reached basement in %d step(s)\n" steps
      else
        walk_to_basement (floor + (stream |> Stream.next |> up_or_down)) (steps + 1) stream
    in
    Stream.of_chars ch |> walk_to_basement 0 0
  )
;;

part1 ();;
part2 ();;
