#use "./extensions.ml";;

type range = { x0:int; y0:int; x1:int; y1:int };;
type instruction =
  | TurnOn of range
  | TurnOff of range
  | Toggle of range
;;

let parse_instruction line =
  Scanf.sscanf line "%[^0-9]%d,%d through %d,%d" (fun cmd x0 y0 x1 y1 ->
    let range = {x0; y0; x1; y1} in
    match (String.trim cmd) with
    | "turn on" -> TurnOn(range)
    | "turn off" -> TurnOff(range)
    | "toggle" -> Toggle(range)
    | _ -> assert false
  )
;;

let adjust lights r fn =
  for y = r.y0 to r.y1 do
    for x = r.x0 to r.x1 do
      lights.(y * 1000 + x) <- fn lights.(y * 1000 + x)
    done
  done; lights
;;

let execute1 ins lights =
  match ins with
  | TurnOn(r) -> adjust lights r (fun _ -> 1)
  | TurnOff(r) -> adjust lights r (fun _ -> 0)
  | Toggle r -> adjust lights r (fun v -> if v = 1 then 0 else 1)
;;

let part1 () =
  File.open_in "day06.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.map parse_instruction
    |> Stream.fold execute1 (Array.make (1000*1000) 0)
    |> Array.fold_left (+) 0
    |> Printf.printf "part1: %d light(s) where lit\n"
  )
;;

let execute2 ins lights =
    match ins with
    | TurnOn(r) -> adjust lights r (fun v -> v + 1)
    | TurnOff(r) -> adjust lights r (fun v -> max (v - 1) 0)
    | Toggle r -> adjust lights r (fun v -> v + 2)
;;

let part2 () =
  File.open_in "day06.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.map parse_instruction
    |> Stream.fold execute2 (Array.make (1000*1000) 0)
    |> Array.fold_left (+) 0
    |> Printf.printf "part2: %d total brightness\n"
  )
;;

part1 ();;
part2 ();;
