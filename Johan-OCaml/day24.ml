#use "./extensions.ml";;

let input = [1;2;3;7;11;13;17;19;23;31;37;41;43;47;53;59;61;67;71;73;79;83;89;97;101;103;107;109;113];;

let example = [1;2;3;4;5;7;8;9;10;11];;

let rec comb n l =
  match n, l with
  | 0, _ -> [[]]
  | _, [] -> []
  | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs
;;

let qe = List.fold_left ( * ) 1;;
let smallest_of groups =
  let l = groups |> List.minf (fun g -> List.length g) |> List.length in
  (fun g -> List.length g = l)
;;

let sum = List.fold_left ( + ) 0;;

let minQE nums grps size =
  let goal = sum nums / grps in
  let groups = comb size nums
  |> List.filter (fun g -> sum g = goal) in
  groups
  |> List.filter (smallest_of groups)
  |> List.map qe
  |> List.min
;;

let part1 () =
  minQE input 3 6
  |> Printf.printf "part1: QE=%d\n%!"
;;

let part2 () =
  minQE input 4 4
  |> Printf.printf "part2: QE=%d\n%!"
;;

part1 ();;
part2 ();;
