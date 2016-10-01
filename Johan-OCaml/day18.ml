#use "./extensions.ml";;
#load "str.cma";;
open Printf;;


module Grid (Grid:sig val width:int end) = struct
  let parse lines =
    lines
    |> List.map (fun line ->
      line |> String.to_list |> List.map (function
        | '#' -> `On
        | '.' -> `Off
        | s -> failwith (sprintf "invalid state: '%c'" s)
      )
    ) |> List.flatten |> Array.of_list
  ;;

  let dump state =
    state |> Array.iteri (fun i x ->
      if i mod Grid.width = 0 then printf "\n";
      match x with
      | `Off -> print_char '.'
      | `On -> print_char '#'
    ); printf "\n%!";
    state
  ;;

  let idx_to_coord i = i mod Grid.width, i / Grid.width;;
  let coord_to_idx (x,y) = y * Grid.width + x;;

  let is_corner i =
    let x,y = idx_to_coord i in
    (x = 0 && y = 0)
    || (x = Grid.width - 1 && y = 0)
    || (x = Grid.width - 1 && y = Grid.width - 1)
    || (x = 0 && y = Grid.width - 1)
  ;;

  let neighbors =
    let off = [|
      -1,-1; +0,-1; +1,-1;
      -1,+0;        +1,+0;
      -1,+1; +0,+1; +1,+1;
    |] in
    Array.init (Grid.width * Grid.width) (fun i ->
      off |> Array.map (fun (ox,oy) ->
        let ix,iy = idx_to_coord i in
        let nx,ny = (ix+ox,iy+oy) in
        if nx < 0 || nx >= Grid.width || ny < 0 || ny >= Grid.width then None
        else Some (coord_to_idx (nx,ny))
      )
    )
  ;;

  let count_on state =
    let rec count i total =
      if i = 0 then total
      else match state.(i - 1) with
        | `On -> count (i - 1) (total + 1)
        | `Off -> count (i - 1) total
    in
    count (Array.length state) 0
  ;;

  let step state =
    let lookup i =
      neighbors.(i) |> Array.map (function
        | None -> `Off
        | Some idx -> state.(idx)
      )
    in
    state |> Array.mapi (fun i s ->
      let neighbors_on = count_on (lookup i) in
      match s with
      | `On -> if neighbors_on = 2 || neighbors_on = 3 then `On else `Off
      | `Off -> if neighbors_on = 3 then `On else `Off
    )
  ;;

  let step_with_fixed_corners state =
    let lookup i =
      neighbors.(i) |> Array.map (function
        | None -> `Off
        | Some idx -> if is_corner idx then `On else state.(idx)
      )
    in
    state |> Array.mapi (fun i s ->
      if is_corner i then `On
      else
        let neighbors_on = count_on (lookup i) in
        match s with
        | `On -> if neighbors_on = 2 || neighbors_on = 3 then `On else `Off
        | `Off -> if neighbors_on = 3 then `On else `Off
    )
  ;;

  let rec animate step steps state =
    if steps = 0 then state
    else animate step (steps - 1) (step state)
  ;;
end



let test_state = "
.#.#.#
...##.
#....#
..#...
#.#..#
####..
" |> Str.split (Str.regexp "\n");;


module Grid6 = Grid(struct let width = 6 end);;
module Grid100 = Grid(struct let width = 100 end);;

let test1 () =
  printf "--- test1 ---";
  let on = test_state |> Grid6.parse
    |> Grid6.animate Grid6.step 4
    |> Grid6.dump
    |> Grid6.count_on
  in
  printf "\n%!";
  assert (on = 4);
;;

let part1 () =
  File.open_in "day18.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.to_list
    |> Grid100.parse
    |> Grid100.animate Grid100.step 100
    |> Grid100.count_on
    |> printf "\npart1: %d lights are lit\n%!"
  )
;;

let test2 () =
  printf "--- test2 ---";
  let on = test_state |> Grid6.parse
    |> Grid6.animate Grid6.step_with_fixed_corners 5
    |> Grid6.dump
    |> Grid6.count_on
  in
  printf "\n%!";
  assert (on = 17);
;;

let part2 () =
  File.open_in "day18.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.to_list
    |> Grid100.parse
    |> Grid100.animate Grid100.step_with_fixed_corners 100
    |> Grid100.count_on
    |> printf "\npart2: %d lights are lit\n%!"
  )
;;

test1 ();;
part1 ();;
test2 ();;
part2 ();;
