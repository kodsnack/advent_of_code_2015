#use "./extensions.ml";;

let to_ints = List.map int_of_char;;
let to_chars = List.map char_of_int;;

let next pwd_ints =
  let first = int_of_char 'a' and last = int_of_char 'z' in
  let rec inc = function
    | [] -> []
    | h :: t ->
      if h != last then (h + 1) :: t
      else first :: inc t
  in
  pwd_ints |> List.rev |> inc |> List.rev
;;

let rec has_no_forbidden = function
  | [] -> true
  | h :: t ->
    let i = int_of_char 'i'
    and o = int_of_char 'o'
    and l = int_of_char 'l' in
    if h = i || h = o || h = l then false
    else has_no_forbidden t
;;

let rec has_straight = function
  | [] | [_] | [_;_] -> false
  | h :: (h' :: h'' :: _ as t) ->
    if h'' = (h' + 1) && h' = (h + 1) then true
    else has_straight t
;;

let has_two_pairs pwd_ints =
  let rec find p = function
    | [] | [_] -> None
    | h :: (h' :: _ as t) ->
      match p with
      | None -> if h = h' then Some(h) else find p t
      | Some(x) -> if h = h' && h != x then Some(h) else find p t
  in
  match find (find None pwd_ints) pwd_ints with
  | None -> false
  | Some(_) -> true
;;

let rec next_valid pwd_ints =
  let next_pwd = next pwd_ints in
  let ok = [has_no_forbidden; has_straight; has_two_pairs]
  |> List.for_all (fun p -> p next_pwd) in
  if ok then next_pwd
  else next_valid next_pwd
;;


let test () =
  [
    ("aaaa", "aaab");
    ("aaaz", "aaba");
    ("azzz", "baaa");
  ] |> List.iter (fun (input, expected) ->
      let actual = input |> String.to_list |> to_ints |> next |> to_chars |> String.from_list in
      if actual = expected then ()
      else failwith (Printf.sprintf "expected: '%s', was: '%s'" expected actual)
    );
  [
    ("abcdefgh", "abcdffaa");
    ("ghijklmn", "ghjaabcc");
  ]
  |> List.iter (fun (input, expected) ->
      let actual = input |> String.to_list |> to_ints |> next_valid |> to_chars |> String.from_list in
      if actual = expected then ()
      else failwith (Printf.sprintf "expected: '%s', was: '%s'" expected actual)
  );
  print_endline "all tests ok."
;;

let input = "vzbxkghb";;

let part1 () =
  input |> String.to_list |> to_ints |> next_valid |> to_chars |> String.from_list
  |> Printf.printf "part1: new password is %s\n"
;;

let part2 () =
  input |> String.to_list |> to_ints |> next_valid |> next_valid |> to_chars |> String.from_list
  |> Printf.printf "part2: new password is %s\n"
;;

test ();;
part1 ();;
part2 ();;
