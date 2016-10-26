#use "./extensions.ml";;

(*
   | 1   2   3   4   5   6
---+---+---+---+---+---+---+
 1 |  1   3   6  10  15  21
 2 |  2   5   9  14  20
 3 |  4   8  13  19
 4 |  7  12  18
 5 | 11  17
 6 | 16
*)

let row = 2981 and column = 3075;;

let tri n = (n * n + n) / 2;;
let lookup_tri r c = tri (r + c - 1) - r + 1;;

let rec iter f s n =
  if n = 1 then s
  else iter f (f s) (n - 1)
;;
let code_for = iter (fun code -> (code * 252533) mod 33554393) 20151125;;

let test1 () =
  Seq.range 1 6
  |> Seq.iter (fun r ->
     Seq.range 1 (7 - r)
     |> Seq.map (fun c ->
        let n = lookup_tri r c in
        n, code_for n
     ) |> Seq.to_list
     |> List.map (fun (n,c) -> Printf.sprintf "(%d: %d)" n c) |> String.concat ";"
     |> Printf.printf "[%s]\n"
  )
;;

let part1 () =
  code_for (lookup_tri row column)
  |> Printf.printf "part1: the code is %d\n%!"
;;

test1 ();;
part1 ();;
