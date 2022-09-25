(* https://adventofcode.com/2018/day/15 *)
type stats = { attack : int; mutable hit : int; is_elf : bool }
type map = { units : (Pos.t, stats) Hashtbl.t; tbl : char array array }

let cnt ?(elf_attack = 3) () =
  let open Hashtbl in
  let add tbl pos is_elf =
    replace tbl pos
      { attack = (if is_elf then elf_attack else 3); hit = 200; is_elf }
  in
  let units = create 64 in
  let y = ref (-1) in
  let parse_line (l : string) =
    incr y;
    Lib.string_2_char_list l |> Array.of_list
    |> Array.mapi (fun x ch ->
           let pos : Pos.t = { x; y = !y } in
           (match ch with
           | 'E' -> add units pos true
           | 'G' -> add units pos false
           | _ -> ());
           if ch = '.' then '.' else '#')
  in
  let tbl =
    Lib.read_file "18" "15" Fun.id |> Array.of_list |> Array.map parse_line
  in
  { tbl; units }

module P1 = struct
  open Hashtbl

  let count_elves units =
    fold (fun _ v acc -> acc + Lib.bool_to_int v.is_elf) units 0

  let stop { units; _ } =
    let elves_nb = count_elves units in
    elves_nb = 0 || elves_nb = length units

  let ordered_list tbl =
    List.sort (fun (a, _) (b, _) -> Pos.compare a b) (to_seq tbl |> List.of_seq)

  let diff_unit_type u1 u2 = u1.is_elf <> u2.is_elf

  let attack_neigh (tbl : char array array) units stats new_pos =
    let goal_pos, goal =
      List.filter_map
        (fun e ->
          let pos = Pos.move_in_square new_pos e in
          match find_opt units pos with
          | Some e when diff_unit_type e stats -> Some (pos, e)
          | _ -> None)
        [ N; W; E; S ]
      |> List.fold_left
           (fun acc e -> if (snd e).hit < (snd acc).hit then e else acc)
           (Pos.zero, { is_elf = false; hit = max_int; attack = -1 })
    in
    if goal.attack = -1 then false
    else (
      goal.hit <- goal.hit - stats.attack;
      if goal.hit <= 0 then (
        remove units goal_pos;
        tbl.(goal_pos.y).(goal_pos.x) <- '.');
      true)

  let move { tbl; units } (unit_pos, stats) =
    if stats.hit > 0 && not (attack_neigh tbl units stats unit_pos) then (
      Hashtbl.iter
        (fun (k : Pos.t) v ->
          if diff_unit_type v stats then tbl.(k.y).(k.x) <- '.')
        units;
      tbl.(unit_pos.y).(unit_pos.x) <- '.';
      let obj_pos_list, dist =
        Pos.find_nearest tbl unit_pos
          (ordered_list units
          |> List.filter (fun (_, e) -> diff_unit_type e stats)
          |> List.map fst)
      in
      match dist with
      | -1 ->
          tbl.(unit_pos.y).(unit_pos.x) <- '#';
          Hashtbl.iter
            (fun (k : Pos.t) v ->
              if diff_unit_type v stats then tbl.(k.y).(k.x) <- '#')
            units
      | _ ->
          let obj_pos = List.sort Pos.compare obj_pos_list |> List.hd in
          let new_pos =
            Pos.move_in_square unit_pos
            @@ List.find
                 (fun dir ->
                   let p = Pos.move_in_square unit_pos dir in
                   Pos.shortest_path_len tbl p obj_pos = dist - 1)
                 [ N; W; E; S ]
          in
          remove units unit_pos;
          replace units new_pos stats;
          Hashtbl.iter
            (fun (k : Pos.t) v ->
              if diff_unit_type v stats then tbl.(k.y).(k.x) <- '#')
            units;
          tbl.(new_pos.y).(new_pos.x) <- '#';
          attack_neigh tbl units stats new_pos |> ignore)

  let main cnt =
    let i = ref 0 in
    (try
       while not (stop cnt) do
         List.iteri
           (fun pos e ->
             if stop cnt && pos + 1 <> length cnt.units then raise Exit
             else move cnt e)
           (ordered_list cnt.units);
         incr i
       done
     with Exit -> ());
    !i * fold (fun _ v acc -> acc + v.hit) cnt.units 0
end

module P2 = struct
  let main () =
    let rec aux elf_attack =
      let content = cnt ~elf_attack () in
      let tot_elf = P1.count_elves content.units in
      let score = P1.main content in
      if P1.count_elves content.units <> tot_elf then aux (elf_attack + 1)
      else score
    in
    aux 4
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main () |> string_of_int
