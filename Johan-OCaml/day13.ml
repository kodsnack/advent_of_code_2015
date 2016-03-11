#use "./extensions.ml";;
module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

let add_rule who next_to change rules =
  let personal_rules = try rules |> StringMap.find who with _ -> StringMap.empty in
  rules |> StringMap.add who (personal_rules |> StringMap.add next_to change)
;;

let dump rules =
  rules
  |> StringMap.iter (fun who rules ->
     rules
     |> StringMap.iter (fun next_to change ->
        Printf.printf "%s:%s -> %d\n" who next_to change
     )
  )
;;

let parse line rules =
  Scanf.sscanf line "%s would %s %d happiness units by sitting next to %[^\\.]" (fun who gain_or_lose units next_to ->
    let change = match gain_or_lose with
      | "gain" -> units
      | "lose" -> -units
      | _ -> failwith (Printf.sprintf "Invalid change: %s" gain_or_lose)
    in
    add_rule who next_to change rules
  )
;;

let eval rules seating =
  let lookup who next_to =
    rules |> StringMap.find who |> StringMap.find next_to
  in

  let rec eval first sum = function
    | [] -> 0
    | [last] -> sum + (lookup last first) + (lookup first last)
    | current :: (next :: _ as rest) -> eval first (sum + (lookup current next) + (lookup next current)) rest
  in

  match seating with
  | [] | [_] -> 0
  | first :: _ -> eval first 0 seating
;;

let permutations seating =
  let p = Array.of_list seating in
  let n = Array.length p in

  let next_perm () =
    let i = let rec aux i =
      if (i < 0) || (p.(i) < p.(i+1)) then i
      else aux (i - 1) in aux (n - 2) in
    let rec aux j k = if j < k then
      let t = p.(j) in
        p.(j) <- p.(k);
        p.(k) <- t;
        aux (j + 1) (k - 1)
    else () in aux (i + 1) (n - 1);
    if i < 0 then false else
      let j = let rec aux j =
        if p.(j) > p.(i) then j
        else aux (j + 1) in aux (i + 1) in
      let t = p.(i) in
        p.(i) <- p.(j);
        p.(j) <- t;
        true
  in

	let next i =
		if next_perm () then Some(Array.to_list p)
    else None
  in

  Stream.from next
;;

let optimize seating rules =
  permutations seating
  |> Stream.map (eval rules)
  |> Stream.max
;;


let test1 () =
  let rules =
    StringMap.empty
    |> add_rule "Alice" "Bob" 54
    |> add_rule "Alice" "Carol" (-79)
    |> add_rule "Alice" "David" (-2)
    |> add_rule "Bob" "Alice" 83
    |> add_rule "Bob" "Carol" (-7)
    |> add_rule "Bob" "David" (-63)
    |> add_rule "Carol" "Alice" (-62)
    |> add_rule "Carol" "Bob" 60
    |> add_rule "Carol" "David" 55
    |> add_rule "David" "Alice" 46
    |> add_rule "David" "Bob" (-7)
    |> add_rule "David" "Carol" 41
  in
  let seating =
    rules |> StringMap.bindings |> List.map (fun (k, _) -> k)
  in
  let total_change = optimize seating rules in
  assert (total_change = 330);
  Printf.printf "test1: total change: %d\n" total_change
;;
test1 ();;


let part1 () =
  File.open_in "day13.input" (fun ch ->
    let rules =
      Stream.of_lines ch
      |> Stream.fold parse StringMap.empty
    in
    let seating =
      rules |> StringMap.bindings |> List.map (fun (k, _) -> k)
    in
    let total_change = optimize seating rules in
    Printf.printf "part1: total change: %d\n" total_change
  )
;;
part1 ();;


let part2 () =
  File.open_in "day13.input" (fun ch ->
    let rules =
      Stream.of_lines ch
      |> Stream.fold parse StringMap.empty
    in
    let guests =
      rules |> StringMap.bindings |> List.map (fun (k, _) -> k)
    in
    let rules' = guests |> List.fold_left (fun r g -> r |> add_rule "me" g 0 |> add_rule g "me" 0) rules in
    let seating = "me" :: guests in
    let total_change = optimize seating rules' in
    Printf.printf "part2: total change: %d\n" total_change
  )
;;
part2 ();;
