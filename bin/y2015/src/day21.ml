(* https://adventofcode.com/2015/day/21 *)
open Lib
open Lib.List

type character = { hit : int; damage : int; armor : int }

(* Note: hit attribute in dico list represents the cost *)
type store = {
  weapon : character list;
  armor : character list;
  ring : character list;
}

let monster =
  let inp =
    read_file "15" "21" (fun e ->
        String.split_on_char ':' e |> tl |> hd |> String.trim |> int_of_string)
  in
  { hit = hd inp; damage = hd (tl inp); armor = hd (tl (tl inp)) }

let hero = { hit = 100; damage = 0; armor = 0 }

let store =
  {
    weapon =
      [
        { hit = 8; damage = 4; armor = 0 };
        { hit = 10; damage = 5; armor = 0 };
        { hit = 25; damage = 6; armor = 0 };
        { hit = 40; damage = 7; armor = 0 };
        { hit = 74; damage = 8; armor = 0 };
      ];
    armor =
      [
        { hit = 13; damage = 0; armor = 1 };
        { hit = 31; damage = 0; armor = 2 };
        { hit = 53; damage = 0; armor = 3 };
        { hit = 75; damage = 0; armor = 4 };
        { hit = 102; damage = 0; armor = 5 };
      ];
    ring =
      [
        { hit = 25; damage = 1; armor = 0 };
        { hit = 50; damage = 2; armor = 0 };
        { hit = 100; damage = 3; armor = 0 };
        { hit = 20; damage = 0; armor = 1 };
        { hit = 40; damage = 0; armor = 2 };
        { hit = 80; damage = 0; armor = 3 };
      ];
  }

let empty = { hit = 0; damage = 0; armor = 0 }

module P1 = struct
  let make_turn ((hero : character), (monster : character)) is_player :
      character * character =
    if is_player then
      ( hero,
        {
          monster with
          hit = min (monster.hit - 1) (monster.hit - hero.damage + monster.armor);
        } )
    else
      ( {
          hero with
          hit = min (hero.hit - 1) (hero.hit - monster.damage + hero.armor);
        },
        monster )

  let rec all_turn ?(is_player = true) (hero, monster) =
    if hero.hit <= 0 then false
    else if monster.hit <= 0 then true
    else
      all_turn ~is_player:(not is_player) (make_turn (hero, monster) is_player)

  let all_combo =
    let weapon_nb, armor_nb, ring_nb =
      (length store.weapon - 1, length store.armor - 1, length store.ring - 1)
    in
    let rec aux wn an rn1 rn2 =
      if wn = -1 then []
      else if an = -2 then aux (wn - 1) armor_nb ring_nb (ring_nb - 1)
      else if rn1 = -2 then
        (wn, an, -1, -1) :: aux wn (an - 1) ring_nb (ring_nb - 1)
      else if rn2 = -2 then aux wn an (rn1 - 1) (rn1 - 2)
      else (wn, an, rn1, rn2) :: aux wn an rn1 (rn2 - 1)
    in
    aux weapon_nb armor_nb ring_nb (ring_nb - 1)

  let main ?(funct = min) ?(cmp = max_int) ?(should_win = Fun.id) monster =
    let aux cost (weapon_ind, armor_ind, r1_ind, r2_ind) =
      let weapon = nth store.weapon weapon_ind in
      let armor = if armor_ind = -1 then empty else nth store.armor armor_ind in
      let ring1 = if r1_ind = -1 then empty else nth store.ring r1_ind in
      let ring2 = if r2_ind = -1 then empty else nth store.ring r2_ind in
      let cost1 = weapon.hit + armor.hit + ring1.hit + ring2.hit in
      let hero =
        {
          damage = weapon.damage + armor.damage + ring1.damage + ring2.damage;
          armor = weapon.armor + armor.armor + ring1.armor + ring2.armor;
          hit = hero.hit;
        }
      in
      let win = all_turn (hero, monster) in
      if should_win win then funct cost cost1 else cost
    in
    fold_left aux cmp all_combo
end

module P2 = struct
  let main monster = P1.main ~funct:max ~cmp:min_int ~should_win:not monster
end

let part1 () = P1.main monster |> print_int
let part2 () = P2.main monster |> print_int
