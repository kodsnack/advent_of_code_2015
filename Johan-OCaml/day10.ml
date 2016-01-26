#use "./extensions.ml";;

let rle digits =
  let enc c n = Printf.sprintf "%d%c" n c |> String.to_list |> List.rev in
  let rec build c n encoded = function
    | [] -> (enc c n) @ encoded |> List.rev
    | c' :: t when c' = c -> build c (n + 1) encoded t
    | c' :: t -> build c' 1 ((enc c n) @ encoded) t
  in
  match digits with
  | [] -> []
  | c :: t -> build c 1 [] t
;;

let rec repeat times f v =
  if times = 0 then v
  else repeat (times - 1) f (f v)
;;

let test () =
  [
    ("1", "11");
    ("11", "21");
    ("21", "1211");
    ("1211", "111221");
    ("111221", "312211");
  ]
  |> List.iter (fun (input, expected) -> assert (input |> String.to_list |> rle |> String.from_list = expected));
  assert ("112" |> String.to_list |> repeat 2 rle |> String.from_list = "122112");
  print_endline "all tests ok."
;;

let input = "1113222113";;

let part1 () =
  input
  |> String.to_list
  |> repeat 40 rle
  |> List.length
  |> Printf.printf "part1: length is %d after 40 rounds.\n"
;;

let part2 () =
  input
  |> String.to_list
  |> repeat 50 rle
  |> List.length
  |> Printf.printf "part2: length is %d after 50 rounds.\n"
;;

test ();;
part1 ();;
part2 ();;
