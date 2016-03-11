#use "./extensions.ml";;

module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

let add_route from dest dist routes =
  let add from dest dist routes =
    let destinations = try StringMap.find from routes with Not_found -> [] in
    StringMap.add from ((dest, dist) :: destinations) routes
  in
  routes
  |> add from dest dist
  |> add dest from dist
;;

let shortest = List.min;;
let longest = List.max;;

let determine kind routes =
  let rec travel visited from =
    let destinations =
      routes
      |> StringMap.find from
      |> List.filter (fun (city, _) -> not (StringSet.mem city visited))
      |> List.map (fun (city, dist) -> dist + travel (StringSet.add from visited) city)
    in
    match destinations with
    | [] -> 0
    | _ -> destinations |> kind
  in
  routes
  |> StringMap.keys
  |> List.map (travel StringSet.empty)
  |> kind
;;

let parse line routes =
  Scanf.sscanf line "%s to %s = %d" (fun from dest dist -> (from, dest, dist))
  |> (fun (from, dest, dist) -> routes |> add_route from dest dist)
;;

let test_input = StringMap.(
  empty
  |> add_route "London" "Dublin"  464
  |> add_route "London" "Belfast" 518
  |> add_route "Dublin" "Belfast" 141
)
;;

let part1_test () =
  test_input
  |> determine shortest
  |> (fun dist ->
        Printf.printf "part1 (test): shortest distance is %d\n" dist;
        assert (dist = 605)
     )
;;

let part2_test () =
  test_input
  |> determine longest
  |> (fun dist ->
        Printf.printf "part2 (test): longest distance is %d\n" dist;
        assert (dist = 982)
     )
;;

let part1 () =
  File.open_in "day09.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.fold parse StringMap.empty
    |> determine shortest
    |> Printf.printf "part1: shortest distance is %d\n"
  )
;;

let part2 () =
  File.open_in "day09.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.fold parse StringMap.empty
    |> determine longest
    |> Printf.printf "part2: longest distance is %d\n"
  )
;;

part1_test ();;
part1 ();;
part2_test ();;
part2 ();;
