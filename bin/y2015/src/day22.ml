(* https://adventofcode.com/2015/day/22 *)
open Lib
open Lib.List

type character = {
  hit : int;
  damage : int;
  mana : int;
  armor : int;
  spent_mana : int;
}

let hero = { hit = 50; mana = 500; spent_mana = 0; damage = 0; armor = 0 }

let monster =
  let inp =
    read_file "15" "22" (fun e ->
        String.split_on_char ':' e |> tl |> hd |> String.trim |> int_of_string)
  in
  { hit = hd inp; damage = hd (tl inp); mana = 0; armor = 0; spent_mana = 0 }

type spell = {
  name : string;
  cost : int;
  time : int;
  effect : character * character -> character * character;
}

let spells_store =
  [
    (let effect (hero, monster) =
       (hero, { monster with hit = monster.hit - 4 })
     in
     { name = "Magic Missle"; cost = 53; time = 0; effect });
    (let effect (hero, monster) =
       ({ hero with hit = hero.hit + 2 }, { monster with hit = monster.hit - 2 })
     in
     { name = "Drain"; cost = 73; time = 0; effect });
    (let effect (hero, monster) =
       ({ hero with armor = hero.armor + 7 }, monster)
     in
     { name = "Shield"; cost = 113; time = 6; effect });
    (let effect (hero, monster) =
       (hero, { monster with hit = monster.hit - 3 })
     in
     { name = "Poison"; cost = 173; time = 6; effect });
    (let effect (hero, monster) =
       ({ hero with mana = hero.mana + 101 }, monster)
     in
     { name = "Recharge"; cost = 229; time = 5; effect });
  ]

module P1 = struct
  let give_cost o_spell = match o_spell with None -> 0 | Some a -> a.cost
  let apply_effect acc e = e.effect acc

  let make_turn ((hero, monster, spells) as params) chosen_spell is_player =
    if hero.hit <= 0 || monster.hit <= 0 then (params, chosen_spell)
    else
      (* If the spell is immediate then it is immediatly append to the list of spells *)
      let spells, new_spell =
        match chosen_spell with
        | Some ({ time; _ } as chosen_spell) when time = 0 ->
            (chosen_spell :: spells, None)
        | _ -> (spells, chosen_spell)
      in
      let hero, monster = fold_left apply_effect (hero, monster) spells in
      let spells =
        filter_map
          (fun e ->
            let res = { e with time = e.time - 1 } in
            if res.time <= 0 then None else Some res)
          spells
      in
      ( ( {
            damage = 0;
            armor = 0;
            hit =
              (if is_player || monster.hit <= 0 then hero.hit
              else min (hero.hit - 1) (hero.hit - monster.damage + hero.armor));
            mana = hero.mana - give_cost chosen_spell;
            spent_mana = hero.spent_mana + give_cost chosen_spell;
          },
          monster,
          spells ),
        new_spell )

  let available_spell { mana; _ } spells =
    filter
      (fun e ->
        e.cost <= mana && not (exists (fun s -> e.name = s.name) spells))
      spells_store
    |> map Option.some

  (* Hero penality is for part 2 *)
  let main ?(hero_penality = id) monster =
    let minim = ref max_int in
    let rec aux (hero, monster, spells) is_player history =
      let hero = if is_player then hero_penality hero else hero in
      if hero.hit <= 0 || hero.spent_mana >= !minim then max_int
      else
        let available_spells =
          if is_player then
            available_spell hero (filter (fun e -> e.time > 1) spells)
          else [ None ]
        in
        fold_left
          (fun acc chosen_spell ->
            min acc
              (let (hero, monster, spells), new_spell =
                 make_turn (hero, monster, spells) chosen_spell is_player
               in
               if hero.hit <= 0 then max_int
               else if monster.hit <= 0 then (
                 minim := min hero.spent_mana !minim;
                 hero.spent_mana)
               else
                 aux
                   ( hero,
                     monster,
                     match new_spell with
                     | None -> spells
                     | Some a -> a :: spells )
                   (not is_player) (chosen_spell :: history)))
          max_int available_spells
    in
    aux (hero, monster, []) true []
end

module P2 = struct
  let main monster =
    P1.main ~hero_penality:(fun h -> { h with hit = h.hit - 1 }) monster
end

let part1 () = P1.main monster |> print_int
let part2 () = P2.main monster |> print_int
