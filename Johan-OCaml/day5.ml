#use "./extensions.ml";;

let has_vowels n s =
  let vowels = String.contains "aeiou" in
  List.of_string s |> List.keep vowels |> List.length >= n

let has_doubled s =
  let chars = Stream.of_string s in
  let first = Stream.next chars in
  let rec scan p n =
    if n = p then true
    else scan n @@ Stream.next chars
  in
  try scan first @@ Stream.next chars
  with Stream.Failure -> false

let has_forbidden s =
  let chars = Stream.of_string s in
  let first = Stream.next chars in
  let rec scan p n =
    match (p,n) with
    | ('a','b') | ('c','d') | ('p','q') | ('x','y') -> true
    | _ -> scan n @@ Stream.next chars
  in
  try scan first @@ Stream.next chars
  with Stream.Failure -> false

let is_nice s =
  (has_vowels 3 s) && (has_doubled s) && not (has_forbidden s)


let part1 =
  File.open_in "day5.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.map (fun s -> if is_nice s then 1 else 0)
    |> Stream.fold (+) 0
    |> Printf.printf "part 1: nice words found %d\n"
  )


let () =
  part1;
