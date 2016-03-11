#use "./extensions.ml";;

let secret = "yzbqklnj";;

let is_coin_of5 hash =
  (String.sub hash 0 5) = "00000"
;;

let is_coin_of6 hash =
  (String.sub hash 0 6) = "000000"
;;

let hash s =
  Digest.(string s |> to_hex)
;;

let generate_from s =
  let rec next i =
    Some((i, Printf.sprintf "%s%d" s i)) in
  Stream.from next
;;

let part1 () =
  generate_from secret
  |> Stream.find (fun (i, s) -> is_coin_of5 @@ hash s)
  |> (fun (i, _) -> Printf.printf "part 1: a 5-digit coin found for %d\n" i)
;;

let part2 () =
  generate_from secret
  |> Stream.find (fun (i, s) -> is_coin_of6 @@ hash s)
  |> (fun (i, _) -> Printf.printf "part 2: a 6-digit coin found for %d\n" i)
;;

part1 ();;
part2 ();;
