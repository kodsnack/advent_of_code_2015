#use "./extensions.ml";;

let has_vowels n s =
  let vowels = String.contains "aeiou" in
  List.of_string s |> List.keep vowels |> List.length >= n
;;

let has_doubled s =
  let rec scan = function
    | [] | [_] -> false
    | h :: (h' :: _ as t) ->
      if h = h' then true
      else scan t
  in
  scan @@ List.of_string s
;;

let has_forbidden s =
  let rec scan = function
    | [] | [_] -> false
    | h :: (h' :: _ as t) ->
      match (h,h') with
      | ('a','b') | ('c','d') | ('p','q') | ('x','y') -> true
      | _ -> scan t
  in
  scan @@ List.of_string s
;;

let has_repeated s =
  let rec scan = function
    | [] | [_] | [_;_] -> false
    | h :: (_ :: (h' :: _) as t) ->
      if h = h' then true
      else scan t
  in
  scan @@ List.of_string s
;;

let has_pair_twice s =
  let rec find p = function
    | [] | [_] -> false
    | h :: (h' :: _ as t) ->
      if (h, h') = p then true
      else find p t
  in
  let rec scan = function
    | [] | _ :: [] -> false
    | x when List.length x < 4 -> false
    | h :: (h' :: t' as t) ->
      if find (h, h') t' then true
      else scan t
  in
  scan @@ List.of_string s
;;

let is_nice1 s =
  (has_vowels 3 s) && (has_doubled s) && not (has_forbidden s)
;;

let is_nice2 s =
  (has_repeated s) && (has_pair_twice s)
;;

let part1 () =
  File.open_in "day05.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.filter is_nice1
    |> Stream.to_list
    |> List.length
    |> Printf.printf "part 1: %d nice words found\n"
  )
;;

let part2 () =
  File.open_in "day05.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.filter is_nice2
    |> Stream.to_list
    |> List.length
    |> Printf.printf "part 2: %d nice words found\n"
  )
;;

part1 ();;
part2 ();;
