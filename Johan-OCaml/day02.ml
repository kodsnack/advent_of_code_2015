#use "./extensions.ml";;

type box = { l: int; w: int; h: int };;
let parse_box line =
  Scanf.sscanf line "%dx%dx%d" (fun l w h ->
    { l=l; w=w; h=h }
  )
;;

let double = ( * ) 2;;

let area_of b =
  let sides = [b.l * b.w; b.w * b.h; b.h * b.l] in
  let smallest = List.min sides in
  sides
  |> List.map double
  |> List.fold_left ( + ) smallest
;;

let ribbon_length_for b =
  let for_bow = b.l * b.w * b.h
  and smallest_perimeter =
    [b.l + b.w; b.w + b.h; b.h + b.l]
    |> List.map double
    |> List.min
  in
  smallest_perimeter + for_bow
;;

let part1 () =
  File.open_in "day02.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.map parse_box
    |> Stream.map area_of
    |> Stream.fold ( + ) 0
    |> Printf.printf "part 1: %d sq feet of paper needed\n"
  )
;;

let part2 () =
  File.open_in "day02.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.map parse_box
    |> Stream.map ribbon_length_for
    |> Stream.fold ( + ) 0
    |> Printf.printf "part 2: %d feet of ribbon needed\n"
  )
;;

part1 ();;
part2 ();;
