#use "./extensions.ml";;

type player = { hp: int; ac: int; mp: int };;
type boss = { hp: int; dmg: int };;

let drain mp (player:player) = { player with mp=player.mp - mp };;
let heal hp (player:player) = { player with hp=player.hp + hp };;
let hit dmg (player:player) = { player with hp=player.hp - (max 1 (dmg - player.ac)) };;
let hurt dmg boss = { boss with hp=boss.hp - dmg };;

type spell_effect = player * boss -> player * boss;;
type spell = { name: string; cost: int; duration: int; effect: spell_effect };;
type round = { player: player; boss: boss; effects: (string * int * spell_effect) list };;

let cast spell round =
  { round with
    player=round.player |> drain spell.cost;
    effects=(spell.name, spell.duration, spell.effect) :: round.effects
  }
;;

let apply_effects round =
  let combatants = { round.player with ac=0 }, round.boss in
  let player, boss = round.effects |> List.fold_left (fun c (_, _, effect) -> effect c) combatants in
  let effects =
    round.effects
    |> List.map (fun (name, ttl, effect) -> name, ttl - 1, effect)
    |> List.filter (fun (_, ttl, _) -> ttl > 0)
  in
  { player; boss; effects }
;;

module Spellbook = struct
  let magic_missile = { name="magic missile"; cost=53; duration=0; effect=(fun (player, boss) -> player, boss |> hurt 4) };;
  let drain = { name="drain"; cost=73; duration=0; effect=(fun (player, boss) -> player |> heal 2, boss |> hurt 2) };;
  let shield = { name="shield"; cost=113; duration=6; effect=(fun (player, boss) -> { player with ac=7 }, boss) };;
  let poison = { name="poison"; cost=173; duration=6; effect=(fun (player, boss) -> player, boss |> hurt 3) };;
  let recharge = { name="recharge"; cost=229; duration=5; effect=(fun (player, boss) -> { player with mp=player.mp + 101 }, boss) };;
  let all = [magic_missile; drain; shield; poison; recharge];;
  let cheapest = all |> List.minf (fun s -> s.cost);;
end

let check round =
  if round.player.hp < 1 then `GameOver `Loss
  else if round.boss.hp < 1 then `GameOver `Win
  else if round.player.mp < Spellbook.cheapest.cost then `GameOver `Loss
  else `GameOn round
;;

let combat ?(player_turn_preflight=fun round -> round) player boss =
  let lowest_mana_spent = ref 10_000 in
  let rec player_turn mana_spent round =
    match round |> player_turn_preflight |> apply_effects |> check with
    | `GameOver outcome -> if outcome = `Win then lowest_mana_spent := mana_spent; [outcome, mana_spent]
    | `GameOn round ->
      let not_in_effect spell = round.effects |> List.for_all (fun (name, _, _) -> name <> spell.name) in
      let can_afford spell = spell.cost <= round.player.mp in
      let lower_mana_total spell = spell.cost + mana_spent <= !lowest_mana_spent in
      Spellbook.all
      |> List.filter not_in_effect
      |> List.filter can_afford
      |> List.filter lower_mana_total
      |> List.map (fun spell -> round |> cast spell |> boss_turn (mana_spent + spell.cost))
      |> List.flatten
  and boss_turn mana_spent round =
    match round |> apply_effects |> check with
    | `GameOver outcome -> if outcome = `Win then lowest_mana_spent := mana_spent; [outcome, mana_spent]
    | `GameOn round -> { round with player=round.player |> hit round.boss.dmg } |> player_turn mana_spent
  in
  player_turn 0 { player; boss; effects=[]; }
;;

let part1 () =
  let player = { hp=50; ac=0; mp=500 }
  and boss = { hp=55; dmg=8 } in
  combat player boss
  |> List.filter (function `Win, _ -> true | _ -> false)
  |> List.minf (fun (_, mana) -> mana)
  |> (fun (_, mana) -> Printf.printf "part1: minimal mana spent: %d\n%!" mana)
;;

let part2 () =
  let player = { hp=50; ac=0; mp=500 }
  and boss = { hp=55; dmg=8 } in
  combat player boss ~player_turn_preflight:(fun round -> { round with player=round.player |> hit 1 })
  |> List.filter (function `Win, _ -> true | _ -> false)
  |> List.minf (fun (_, mana) -> mana)
  |> (fun (_, mana) -> Printf.printf "part2: minimal mana spent: %d\n%!" mana)
;;

part1 ();;
part2 ();;
