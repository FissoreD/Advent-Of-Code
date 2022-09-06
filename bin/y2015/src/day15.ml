(* https://adventofcode.com/2015/day/15 *)

let cnt () = Lib.read_file "15" "15" Lib.find_all_ints_in_string

module P1 = struct
  let build_100_combo max =
    let res = ref [] in
    for a = 0 to max do
      for b = 0 to max - a do
        for c = 0 to max - a - b do
          if a + b + c <= max then res := [ a; b; c; max - a - b - c ] :: !res
        done
      done
    done;
    !res

  let calc_op cnt (combo : int list) =
    let open List in
    map2 (fun i l -> map (( * ) i) l) combo cnt
    |> fold_left
         (fun acc e -> map2 ( + ) acc e)
         (init (length (hd cnt)) (Fun.const 0))
    |> map (max 0)
    |> fold_left ( * ) 1

  let main cnt =
    let cnt' = List.map Lib.List.remove_last cnt in
    let combos = build_100_combo 100 in
    List.map (calc_op cnt') combos |> List.fold_left max 0
end

module P2 = struct
  let has_500_calories cnt = List.(map hd cnt |> fold_left ( + ) 0) = 500

  let calc_op cnt (combo : int list) =
    let open List in
    let combo_map = map2 (fun i l -> map (( * ) i) l) combo cnt in
    if has_500_calories combo_map then
      map tl combo_map
      |> fold_left
           (fun acc e -> map2 ( + ) acc e)
           (init (length (hd cnt) - 1) (Fun.const 0))
      |> map (max 0)
      |> fold_left ( * ) 1
    else 0

  let main cnt =
    let cnt' = List.map List.rev cnt in
    let combos = P1.build_100_combo 100 in
    List.map (calc_op cnt') combos |> List.fold_left max 0
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
