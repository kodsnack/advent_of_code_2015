#use "./extensions.ml";;

type actor_stats = {
  hp: int; dmg: int; ac: int;
};;

type actor =
  | Player of actor_stats
  | Boss of actor_stats
;;
let stats_of = function Player s | Boss s -> s;;
let with_stats s = function
  | Player _ -> Player s
  | Boss _ -> Boss s
;;

let rec fight attacker defender =
  let attacker_stats = stats_of attacker
  and defender_stats = stats_of defender in
  let defender_stats' = { defender_stats with
    hp = defender_stats.hp - (max 1 (attacker_stats.dmg - defender_stats.ac))
  } in
  if defender_stats'.hp < 1 then attacker
  else fight (defender |> with_stats defender_stats') attacker
;;

let winning = function
  | Player _ -> true
  | Boss _ -> false
;;

let losing a = not (winning a);;

let tests () =
  let is_winning =
    fight (Player { hp=8; dmg=5; ac=5 }) (Boss { hp=12; dmg=7; ac=2 })
    |> winning
  in
  assert is_winning;
  print_endline "tests passed"
;;

type item_stats = { name:string; cost: int; dmg: int; ac: int };;
let weapons = [
  { name="Dagger";     cost=8;  dmg=4; ac=0 };
  { name="Shortsword"; cost=10; dmg=5; ac=0 };
  { name="Warhammer";  cost=25; dmg=6; ac=0 };
  { name="Longsword";  cost=40; dmg=7; ac=0 };
  { name="Greataxe";   cost=74; dmg=8; ac=0 };
];;

let armours = [
  { name="Leather";    cost=13;  dmg=0; ac=1 };
  { name="Chainmail";  cost=31;  dmg=0; ac=2 };
  { name="Splintmail"; cost=53;  dmg=0; ac=3 };
  { name="Bandedmail"; cost=75;  dmg=0; ac=4 };
  { name="Platemail";  cost=102; dmg=0; ac=5 };
];;

let rings = [
  { name="Damage +1";  cost=25;  dmg=1; ac=0};
  { name="Damage +2";  cost=50;  dmg=2; ac=0};
  { name="Damage +3";  cost=100; dmg=3; ac=0};
  { name="Defense +1"; cost=20;  dmg=0; ac=1};
  { name="Defense +2"; cost=40;  dmg=0; ac=2};
  { name="Defense +3"; cost=80;  dmg=0; ac=3};
];;

let real_boss = Boss { hp=104; dmg=8; ac=1 };;
let player_stats = { hp=100; dmg=0; ac=0 };;

let some = List.map (fun x -> Some x);;

(* generate all allowed outfits *)
let all_outfits =
  some weapons |> List.map (fun w ->
    (None :: some armours) |> List.map (fun a ->
      (None :: some rings) |> List.map (fun r1 ->
        (None :: (some rings -- r1)) |> List.map (fun r2 ->
          [w; a; r1; r2]
        )
      ) |> List.flatten
    ) |> List.flatten
  ) |> List.flatten
;;

let equip (s:actor_stats) = function
  | None -> s
  | Some i -> { s with dmg=s.dmg + i.dmg; ac=s.ac + i.ac }
;;

let cost_of_outfit o =
  o |> List.map (function None -> 0 | Some i -> i.cost)
  |> List.fold_left ( + ) 0
;;

let battle_test expected_outcome outfit =
  let player = Player (outfit |> List.fold_left equip player_stats) in
  fight player real_boss |> expected_outcome
;;

let part1 () =
  all_outfits
  |> List.filter (battle_test winning)
  |> List.map cost_of_outfit
  (* which one is the cheapest? *)
  |> List.sort compare
  |> List.hd
  |> Printf.printf "part1: lowest cost is %d\n%!"
;;

let part2 () =
  all_outfits
  |> List.filter (battle_test losing)
  |> List.map cost_of_outfit
  (* which one is the most expensive? *)
  |> List.sort compare |> List.rev
  |> List.hd
  |> Printf.printf "part2: highest cost is %d\n%!"
;;

tests ();;
part1 ();;
part2 ();;
