#use "./extensions.ml";;

module StringMap = Map.Make(String);;

type signal =
  | Value of int
  | Wire of string
;;

type gate =
  | And of signal * signal
  | Or of signal * signal
  | Not of signal
  | LShift of signal * signal
  | RShift of signal * signal
  | Equal of signal
;;

let solve signals wiring =
  let signals = ref signals in
  let rec solve_rec w =
    try StringMap.find w !signals
    with _ ->
      let max_v = 65535 in
      let signal = function Value(v) -> v | Wire(w) -> solve_rec w in
      let activate w s = signals := StringMap.add w s !signals; s in
      let wire = try StringMap.find w wiring with _ -> failwith ("bad wiring: " ^ w) in
      match wire with
      | Equal(s) -> signal s |> activate w
      | And(s1,s2) -> (signal s1) land (signal s2) |> activate w
      | Or(s1,s2) -> (signal s1) lor (signal s2) |> activate w
      | LShift(s1,s2) -> (lsl) (signal s1) (signal s2) |> activate w
      | RShift(s1,s2) -> (lsr) (signal s1) (signal s2) |> activate w
      | Not(s) -> max_v - (signal s) |> activate w
  in
  wiring |> StringMap.iter (fun k _ -> solve_rec k |> ignore);
  !signals
;;

let test () =
  let verify reg =
    match reg with
    | "d" -> 72    | "e" -> 507   | "f" -> 492 | "g" -> 114
    | "h" -> 65412 | "i" -> 65079 | "x" -> 123 | "y" -> 456
    | _ -> assert false
  in
  Printf.printf "\ntest\n";
  [
    ( "x", (Equal(Value(123))) );
    ( "y", (Equal(Value(456))) );
    ( "d", (And(Wire("x"),Wire("y"))) );
    ( "e", (Or(Wire("x"),Wire("y"))) );
    ( "f", (LShift(Wire("x"),Value(2))) );
    ( "g", (RShift(Wire("y"),Value(2))) );
    ( "h", (Not(Wire("x"))) );
    ( "i", (Not(Wire("y"))) );
  ]
  |> StringMap.from_list
  |> solve StringMap.empty
  |> StringMap.iter (fun k v -> assert ((verify k) = v); Printf.printf "%s: %d\n" k v)
;;

let parse_instruction line =
  Scanf.sscanf line "%[a-z0-9] %[A-Z] %[a-z0-9] -> %[a-z]" (fun s1 g s2 w ->
    let signal s = try Value(int_of_string s) with _ -> Wire(s) in
    let gate = match g with
      | "AND" -> And((signal s1), (signal s2))
      | "OR" -> Or((signal s1), (signal s2))
      | "NOT" -> Not((signal s2))
      | "LSHIFT" -> LShift((signal s1), (signal s2))
      | "RSHIFT" -> RShift((signal s1), (signal s2))
      | "" -> Equal((signal s1))
      | _ -> assert false
    in
    (w, gate)
  )
;;

let part1 () =
  File.open_in "day07.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.map parse_instruction
    |> StringMap.from_stream
    |> solve StringMap.empty
    |> StringMap.find "a" |> Printf.printf "part 1: signal on a=%d\n"
  )
;;

let part2 () =
  File.open_in "day07.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.map parse_instruction
    |> StringMap.from_stream
    |> StringMap.add "b" (Equal(Value(3176)))
    |> solve StringMap.empty
    |> StringMap.find "a" |> Printf.printf "part 2: signal on a=%d\n"
  )
;;

part1 ();;
part2 ();;
