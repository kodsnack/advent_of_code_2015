#use "./extensions.ml";;

let rec sum_of_numbers sum = function
  | Json.Object(elements) -> sum +. (elements |> List.fold_left (fun acc (_, e) -> acc +. sum_of_numbers 0. e) 0.)
  | Json.Array(elements)  -> sum +. (elements |> List.fold_left (fun acc e -> acc +. sum_of_numbers 0. e) 0.)
  | Json.Number(num)      -> sum +. num
  | _ -> 0.
;;

let rec sum_of_non_red_numbers sum = function
  | Json.Object(elements) ->
    if elements |> List.exists (fun (_, v) -> match v with Json.String(str) when str = "red" -> true | _ -> false) then 0.
    else sum +. (elements |> List.fold_left (fun acc (_, e) -> acc +. sum_of_non_red_numbers 0. e) 0.)
  | Json.Array(elements)  -> sum +. (elements |> List.fold_left (fun acc e -> acc +. sum_of_non_red_numbers 0. e) 0.)
  | Json.Number(num)      -> sum +. num
  | _ -> 0.
;;

let test1 () =
  [
    ("[1,[2,30],-5]", 28.);
    ("{\"a\":[{\"aa\":2},{\"b\":4}],\"c\":-1}", 5.);
  ]
  |> List.iter (fun (input, expected) ->
      let actual = input |> Json.parse |> sum_of_numbers 0. in
        if actual = expected then ()
        else failwith (Printf.sprintf "expected: %g, was: %g" expected actual)
     );
  print_endline "test1: all ok.";
;;
test1 ();;

let test2 () =
  [
    ("[1,2,3]", 6.);
    ("[1,{\"c\":\"red\",\"b\":2},3]", 4.);
  ]
  |> List.iter (fun (input, expected) ->
      let actual = input |> Json.parse |> sum_of_non_red_numbers 0. in
        if actual = expected then ()
        else failwith (Printf.sprintf "expected: %g, was: %g" expected actual)
     );
  print_endline "test2: all ok.";
;;
test2 ();;

let part1 () =
  File.open_in "day12.input" (fun ch ->
    Stream.of_chars ch
    |> Json.parse_stream
    |> sum_of_numbers 0.
    |> Printf.printf "part 1: the sum is %g\n"
  )
;;
part1 ();;

let part2 () =
  File.open_in "day12.input" (fun ch ->
    Stream.of_chars ch
    |> Json.parse_stream
    |> sum_of_non_red_numbers 0.
    |> Printf.printf "part 2: the sum is %g\n"
  )
;;
part2 ();;
