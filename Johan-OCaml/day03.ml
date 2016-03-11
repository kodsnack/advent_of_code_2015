#use "./extensions.ml";;

module Houses = Map.Make(String);;

let move from instruction =
  let (x, y) = from in
  match instruction with
  | '^' -> (x, y + 1)
  | 'v' -> (x, y - 1)
  | '>' -> (x + 1, y)
  | '<' -> (x - 1, y)
  | _ -> failwith (Printf.sprintf "bad instruction: %c" instruction)
;;

let to_string p =
  let (x,y) = p in
  Printf.sprintf "%dx%d" x y
;;

let deliver instructions =
  let rec follow instructions santa houses =
    try
      let santa = move santa (Stream.next instructions) in
      follow instructions santa (Houses.add (santa |> to_string) true houses)
    with Stream.Failure -> houses
    in
    follow instructions (0,0) (Houses.(empty |> add ((0,0) |> to_string) true))
;;

let deliver2 instructions =
  let rec follow instructions current next houses =
    try
      let current = move current (Stream.next instructions) in
      follow instructions next current (Houses.add (current |> to_string) true houses)
    with Stream.Failure -> houses
  in
  follow instructions (0,0) (0,0) (Houses.(empty |> add ((0,0) |> to_string) true))
;;

let part1 () =
  File.open_in "day03.input" (fun ch ->
    Stream.of_chars ch
    |> deliver
    |> Houses.cardinal
    |> Printf.printf "part 1: %d house(s) visited\n"
  )
;;

let part2 () =
  File.open_in "day03.input" (fun ch ->
    Stream.of_chars ch
    |> deliver2
    |> Houses.cardinal
    |> Printf.printf "part 2: %d house(s) visited\n"
  )
;;

part1 ();;
part2 ();;

