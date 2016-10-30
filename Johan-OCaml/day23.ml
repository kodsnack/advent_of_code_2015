#use "./extensions.ml";;

type register = A | B;;
type instruction =
  | Half of register
  | Triple of register
  | Increment of register
  | Jump of int
  | JumpIfEven of register * int
  | JumpIfOne of register * int
;;

let register_from = function
  | "a" -> A
  | "b" -> B
  | r -> failwith (Printf.sprintf "invalid register: %s" r)
;;

let as_signed_int s = Scanf.sscanf s "%d" (fun d -> d);;

let parse line =
  Scanf.sscanf line "%s %s@, %s" (fun op a b ->
    match op with
    | "hlf" -> Half (register_from a)
    | "tpl" -> Triple (register_from a)
    | "inc" -> Increment (register_from a)
    | "jmp" -> Jump (as_signed_int a)
    | "jie" -> JumpIfEven (register_from a, as_signed_int b)
    | "jio" -> JumpIfOne (register_from a, as_signed_int b)
    | _ -> failwith (Printf.sprintf "parse error: %s" line)
  )
;;

let apply f (a, b) = function A -> (f a), b | B -> a, (f b);;
let half = apply (fun x -> x / 2);;
let triple = apply (( * ) 3);;
let increment = apply (( + ) 1);;
let value_of (a, b) = function A -> a | B -> b;;

let execute regs program =
  let rec next i pc regs =
    let next = next (i + 1) in
    if pc < 0 || pc >= Array.length program then regs
    else
      match program.(pc) with
      | Half r -> r |> half regs |> next (pc + 1)
      | Triple r -> r |> triple regs |> next (pc + 1)
      | Increment r -> r |> increment regs |> next (pc + 1)
      | Jump o -> regs |> next (pc + o)
      | JumpIfEven (r, o) ->
        if (r |> value_of regs) mod 2 = 0 then regs |> next (pc + o)
        else regs |> next (pc + 1)
      | JumpIfOne (r, o) ->
        if (r |> value_of regs) = 1 then regs |> next (pc + o)
        else regs |> next (pc + 1)
  in
  next 0 0 regs
;;


let tests () =
  (* parsing *)
  assert (parse "hlf a" = Half A);;
  assert (parse "tpl b" = Triple B);;
  assert (parse "inc a" = Increment A);;
  assert (parse "jmp 0" = Jump 0);;
  assert (parse "jmp +0" = Jump 0);;
  assert (parse "jmp -0" = Jump 0);;
  assert (parse "jie a, 0" = JumpIfEven (A, 0));;
  assert (parse "jie b, +0" = JumpIfEven (B, 0));;
  assert (parse "jie a, -0" = JumpIfEven (A, 0));;
  assert (parse "jie a, +5" = JumpIfEven (A, 5));;
  assert (parse "jie b, -3" = JumpIfEven (B, -3));;
  assert (parse "jie a, 42" = JumpIfEven (A, 42));;
  assert (parse "jio a, 0" = JumpIfOne (A, 0));;
  assert (parse "jio b, +0" = JumpIfOne (B, 0));;
  assert (parse "jio a, -0" = JumpIfOne (A, 0));;
  assert (parse "jio a, +5" = JumpIfOne (A, 5));;
  assert (parse "jio b, -3" = JumpIfOne (B, -3));;
  assert (parse "jio a, 42" = JumpIfOne (A, 42));;
  (* operations *)
  assert (half (4, 2) A = (2, 2));;
  assert (triple (1, 3) B = (1, 9));;
  assert (increment (1, 1) A = (2, 1));;
  assert (value_of (1, 2) A = 1);;
  assert (value_of (1, 2) B = 2);;
  (* execution *)
  [|
    Increment A;
    JumpIfOne (A, 2);
    Triple A;
    Increment A;
  |] |> execute (0, 0) |> (fun (a, b) -> assert (a = 2));
  [|
    JumpIfOne (A, 3);
    Increment A;
    Jump (-2);
    Increment B;
    JumpIfEven (B, 2);
    Jump (-2);
    Increment A;
  |] |> execute (0, 0) |> (fun (a, b) -> assert (a = 2 && b = 2));

  Printf.printf "all tests passed\n%!"
;;

let part1 () =
  File.open_in "day23.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.map parse
    |> Stream.to_array
    |> execute (0, 0)
    |> (fun (_, b) -> Printf.printf "part1: b=%d\n%!" b)
  )
;;

let part2 () =
  File.open_in "day23.input" (fun ch ->
    Stream.of_lines ch
    |> Stream.map parse
    |> Stream.to_array
    |> execute (1, 0)
    |> (fun (_, b) -> Printf.printf "part2: b=%d\n%!" b)
  )
;;

tests ();;
part1 ();;
part2 ();;
