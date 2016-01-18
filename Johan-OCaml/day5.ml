#use "./extensions.ml";;

let has_vowels n s =
  let vowels = String.contains "aeiou" in
  List.of_string s |> List.keep vowels |> List.length >= n

let has_doubled s =
  let rec scan = function
    | [] | [_] -> false
    | h :: (h' :: _ as t) ->
      if h = h' then true
      else scan t
  in
  scan @@ List.of_string s

let has_forbidden s =
  let rec scan = function
    | [] | [_] -> false
    | h :: (h' :: _ as t) ->
      match (h,h') with
      | ('a','b') | ('c','d') | ('p','q') | ('x','y') -> true
      | _ -> scan t
  in
  scan @@ List.of_string s

let is_nice s =
  (has_vowels 3 s) && (has_doubled s) && not (has_forbidden s)


let part1 =
  File.open_in "day5.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.filter is_nice
    |> Stream.to_list
    |> List.length
    |> Printf.printf "part 1: %d nice words found\n"
  )


let () =
  part1;
