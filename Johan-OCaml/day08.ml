#use "./extensions.ml";;

let to_hex = Printf.printf "%x";;
let from_hex s = Scanf.sscanf s "%x" (fun v -> v);;

let decode s =
  let hex_to_c chars = chars |> String.from_list |> from_hex |> char_of_int in
  let rec decode dst src =
    match src with
    | [] -> dst
    | '"' :: src' -> decode dst src'
    | '\\' :: src' ->
      begin match src' with
      | c :: src'' when c = '"' || c = '\\' -> decode (c :: dst) src''
      | 'x' :: c1 :: c2 :: src'' -> decode (([c1;c2] |> hex_to_c) :: dst) src''
      | _ -> failwith "invalid input"
      end
    | c :: src' -> decode (c :: dst) src'
  in
  s |> String.to_list |> decode [] |> List.rev |> String.from_list
;;

let encode s =
  let rec encode dst src =
    match src with
    | [] -> dst
    | c :: src' ->
      if c = '"' || c = '\\' then encode (c :: '\\' :: dst) src'
      else encode (c :: dst) src'
    in
  s |> String.to_list |> encode [] |> List.rev |> String.from_list |> Printf.sprintf "\"%s\""
;;

let count_decoded strings =
  strings
  |> Stream.map (fun s -> (String.length s, s |> decode |> String.length))
  |> Stream.fold (fun (l1,l2) (t1,t2) -> (t1+l1,t2+l2)) (0,0)
  |> (fun (t1,t2) -> Printf.printf "%d - %d = %d\n" t1 t2 (t1-t2))
;;

let count_encoded strings =
  strings
  |> Stream.map (fun s -> (s |> encode |> String.length, String.length s))
  |> Stream.fold (fun (l1,l2) (t1,t2) -> (t1+l1,t2+l2)) (0,0)
  |> (fun (t1,t2) -> Printf.printf "%d - %d = %d\n" t1 t2 (t1-t2))
;;

let test_input = ["\"\""; "\"abc\""; "\"aaa\\\"aaa\""; "\"\\x27\""; ];;

let test1 () =
  print_string "test1: ";
  test_input
  |> Stream.of_list
  |> count_decoded
;;

let test2 () =
  print_string "test2: ";
  test_input
  |> Stream.of_list
  |> count_encoded
;;

let part1 () =
  print_string "part1: ";
  File.open_in "day08.input" (fun ch ->
    Stream.of_lines ch
    |> count_decoded
  )
;;

let part2 () =
  print_string "part2: ";
  File.open_in "day08.input" (fun ch ->
    Stream.of_lines ch
    |> count_encoded
  )
;;

test1 ();;
part1 ();;
test2 ();;
part2 ();;
