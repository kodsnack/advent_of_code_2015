#use "./extensions.ml";;
#load "str.cma";;

let parse line =
  Scanf.sscanf line "%s %d: %s@\n" (fun name id info ->
    let facts =
      Str.split (Str.regexp ",") info
      |> List.map String.trim
      |> List.map (fun fact -> Scanf.sscanf fact "%s@: %d" (fun f v -> (f, v)))
    in
    (id, facts)
  )
;;

let aunts =
  File.open_in "day16.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.map parse
    |> Stream.to_list
  )
;;

let matching_aunt cmp (_, aunt_facts) =
  [
    "children", 3;
    "cats", 7;
    "samoyeds", 2;
    "pomeranians", 3;
    "akitas", 0;
    "vizslas", 0;
    "goldfish", 5;
    "trees", 3;
    "cars", 2;
    "perfumes", 1;
  ]
  |> List.map (fun (fact, value) ->
      try
        cmp fact value @@ List.assoc fact aunt_facts
      with _ -> true
    )
  |> List.fold_left ( && ) true
;;

let part1_compare fact expected actual =
  actual = expected
;;

let part1 () =
  aunts
  |> List.find (matching_aunt part1_compare)
  |> (fun (id, _) -> Printf.printf "part1: id %d\n%!" id)
;;

let part2_compare fact expected actual =
  match fact with
  | "cats" | "trees" -> actual > expected
  | "pomeranians" | "goldfish" -> actual < expected
  | _ -> actual = expected
;;

let part2 () =
  aunts
  |> List.find (matching_aunt part2_compare)
  |> (fun (id, _) -> Printf.printf "part2: id %d\n%!" id)
;;

part1 ();;
part2 ();;
