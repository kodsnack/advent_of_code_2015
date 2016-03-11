#use "./extensions.ml";;

type reindeer = { name: string; speed: int; flight: int; rest: int };;

let parse line =
  Scanf.sscanf line "%s can fly %d km/s for %d seconds, but then must rest for %d seconds." (fun name speed flight rest ->
    { name; speed; flight; rest }
  )
;;

let race t r =
  let rec flying t =
    if r.flight > t then r.flight - t
    else r.flight + resting (t - r.flight)
  and resting t =
    if r.rest > t then 0
    else flying (t - r.rest)
  in
  (r, r.speed * flying t)
;;


let test_input = [
  { name = "Comet"; speed = 14; flight = 10; rest = 127 };
  { name = "Dancer"; speed = 16; flight = 11; rest = 162 };
];;

let test1 () =
  let winner, dist =
    test_input
    |> List.map (race 1000)
    |> List.maxf (fun (r, d) -> d)
  in
  assert (winner.name = "Comet" && dist = 1120);
  Printf.printf "test1: The winner is %s at %d km.\n" winner.name dist
;;
test1 ();;

let part1 () =
  File.open_in "day14.input" (fun ch ->
    let winner, dist = Stream.of_lines ch
      |> Stream.map parse
      |> Stream.map (race 2503)
      |> Stream.maxf (fun (r, d) -> d)
    in
    Printf.printf "part1: The winner is %s at %d km.\n" winner.name dist
  )
;;
part1 ();;


type mode =
  | Flying  of int
  | Resting of int
;;

let race2 time contestants =
  let move contestants =
    contestants
    |> List.map (fun (score, dist, mode, reindeer) ->
      match mode with
      | Flying  t -> (score, dist + reindeer.speed, (if t = 1 then Resting(reindeer.rest) else Flying(t - 1)), reindeer)
      | Resting t -> (score, dist, (if t = 1 then Flying(reindeer.flight) else Resting(t - 1)), reindeer)
    )
  in
  let rec tick time_left contestants =
    if time_left = 0 then
      contestants |> List.maxf (fun (score, _, _, _) -> score)
    else
      let contestants' = contestants |> move in
      let _, furthest, _ , _ =
        contestants'
        |> List.maxf (fun (_, dist, _, _) -> dist)
      in
      let contestants'' =
        contestants'
        |> List.map (fun (score, dist, mode, reindeer) ->
            (score + (if dist = furthest then 1 else 0), dist, mode, reindeer)
        )
      in
      tick (time_left - 1) contestants''
  in
  contestants
  |> List.map (fun reindeer -> (0, 0, Flying(reindeer.flight), reindeer))
  |> tick time
;;

let test2 () =
  let score, dist, _, winner = test_input |> race2 1000 in
  assert (winner.name = "Dancer" && score = 689);
  Printf.printf "test2: The winner is %s with %d points at %d km.\n" winner.name score dist
;;
test2 ();;

let part2 () =
  File.open_in "day14.input" (fun ch ->
    let score, dist, _, winner = Stream.of_lines ch
      |> Stream.map parse
      |> Stream.to_list
      |> race2 2503
    in
    Printf.printf "part2: The winner is %s with %d points at %d km.\n" winner.name score dist
  )
;;
part2 ();;
