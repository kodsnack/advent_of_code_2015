#use "./extensions.ml";;

type ingredient = {
  name: string;
  capacity: int;
  durability: int;
  flavor: int;
  texture: int;
  calories: int
};;

let parse line =
  Scanf.sscanf line "%s@: capacity %d, durability %d, flavor %d, texture %d, calories %d" (fun name capacity durability flavor texture calories ->
    { name; capacity; durability; flavor; texture; calories }
  )
;;

let sequence_of len =
  let mx = 100 in
  let tot = mx ** len in
  let init n = Array.init len (fun i -> (n / (mx ** i)) mod mx) in
  (fun n ->
    if n < tot then Some(init n |> Array.to_list, n + 1)
    else None
  )
;;

let complete_recipe_proportions proportions =
  List.fold_left ( + ) 0 proportions = 100
;;

let compose_recipe_from = List.map2 (fun ingredient amount -> (amount, ingredient));;

let calculate_score recipe =
  recipe
  |> List.map (fun (a, i) -> [a * i.capacity; a * i.durability; a * i.flavor; a * i.texture])
  |> List.fold_left (List.map2 ( + )) [0;0;0;0]
  |> List.map (Pervasives.max 0)
  |> List.fold_left ( * ) 1
;;

let has_500_calories recipe =
  let calories =
    recipe
    |> List.map (fun (a,i) -> a * i.calories)
    |> List.fold_left ( + ) 0
  in
  calories = 500
;;

let test_ingredients = [
  { name = "Butterscotch"; capacity = -1; durability = -2; flavor = 6; texture = 3; calories = 8 };
  { name = "Cinnamon"; capacity = 2; durability = 3; flavor = -2; texture = -1; calories = 3 };
];;

let test1 () =
  let score =
    [44; 56]
    |> compose_recipe_from test_ingredients
    |> calculate_score
  in
  Printf.printf "test1: score %d\n%!" score;
  assert (score = 62842880)
;;

let test2 () =
  let recipe = [40; 60] |> compose_recipe_from test_ingredients in
  let score = recipe |> calculate_score in
  Printf.printf "test2: score %d\n%!" score;
  assert (score = 57600000);
  assert (recipe |> has_500_calories);
;;

let part1 () =
  File.open_in "day15.input" (fun ch ->
    let ingredients = Stream.of_lines ch
      |> Stream.map parse
      |> Stream.to_list
    in
    Seq.unfold (sequence_of (List.length ingredients)) 0
    |> Seq.filter complete_recipe_proportions
    |> Seq.map (compose_recipe_from ingredients)
    |> Seq.map calculate_score
    |> Seq.max
    |> Printf.printf "part1: highest score: %d\n%!"
  )
;;

let part2 () =
  File.open_in "day15.input" (fun ch ->
    let ingredients = Stream.of_lines ch
      |> Stream.map parse
      |> Stream.to_list
    in
    Seq.unfold (sequence_of (List.length ingredients)) 0
    |> Seq.filter complete_recipe_proportions
    |> Seq.map (compose_recipe_from ingredients)
    |> Seq.filter has_500_calories
    |> Seq.map calculate_score
    |> Seq.max
    |> Printf.printf "part2: highest score with 500 calories: %d\n%!"
  )
;;

test1 ();;
test2 ();;
part2 ();;
