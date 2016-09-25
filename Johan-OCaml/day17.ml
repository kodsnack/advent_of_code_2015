#use "./extensions.ml";;
open Printf;;

let dump items = List.iter (fun combo -> combo |> List.iter (printf "%d "); printf "\n%!") items; items;;

let optimize_for goal items =
  let max_combos = 1 lsl List.length items in
  let nth_bit n i = (n lsr i) land 1 = 1 in
  let rec search n combos =
    if n = max_combos then combos
    else
      let combo = items |> List.filteri (fun i x -> nth_bit n i) in
      if combo |> List.reduce ( + ) 0 = goal then search (n + 1) (combo :: combos)
      else search (n + 1) combos
  in
  search 0 []
;;

let test_containers = [20;15;10;5;5];;
let test1 () =
  let normalize lst =
    lst |> List.map (List.fast_sort compare) |> List.fast_sort compare
  in
  let expected = [
    [20;5];
    [20;5];
    [15;10];
    [15;5;5];
  ] |> normalize
  and optimized =
    test_containers
    |> optimize_for 25
    |> normalize
  in
  assert (optimized = expected)
;;

let containers = [50;44;11;49;42;46;18;32;26;40;21;7;18;43;10;47;36;24;22;40];;

let part1 () =
  containers
  |> optimize_for 150
  |> List.length
  |> printf "part1: %d combination(s) found\n%!"
;;

let part2 () =
  let combos = containers |> optimize_for 150 |> List.map List.length in
  let min = combos |> List.min in
  combos
  |> List.filter (( = ) min)
  |> List.length
  |> printf "part2: %d combination(s) found\n%!"
;;

test1 ();;
part1 ();;
part2 ();;
