(* https://adventofcode.com/2016/day/24 *)

let cnt =
  Lib.read_file "16" "24" (fun e ->
      Array.init (String.length e) (fun pos ->
          if e.[pos] = '#' then None else Some e.[pos]))
  |> Array.of_list

module P1 = struct
  let get_goal_pos_list (s : char option array array) =
    let res : Pos.t list ref = ref [] in
    let start = ref Pos.zero in
    for y = 0 to Array.length s - 1 do
      for x = 0 to Array.length s.(0) - 1 do
        if s.(y).(x) <> None then
          let content = Option.get s.(y).(x) in
          if content = '0' then start := { x; y }
          else if content <> '.' then res := { x; y } :: !res
      done
    done;
    (!res, !start)

  let dists grid goal_pos =
    let pos_arr = goal_pos |> Array.of_list in
    let len = Array.length pos_arr in
    let dist_mat = Array.init len (fun _ -> Array.make len (-1)) in
    for i = 0 to len - 1 do
      for j = i + 1 to len - 1 do
        let dist = Pos.shortest_path_len grid pos_arr.(i) pos_arr.(j) in
        dist_mat.(i).(j) <- dist;
        dist_mat.(j).(i) <- dist
      done
    done;
    dist_mat

  let rec calc_dist dist_mat acc = function
    | [] | [ _ ] -> acc
    | a :: b :: tl -> calc_dist dist_mat (acc + dist_mat.(a).(b)) (b :: tl)

  let dist_from_start grid start goal_pos =
    List.map (fun e -> Pos.shortest_path_len grid start e) goal_pos
    |> Array.of_list

  let find_shortest_path ?(go_back = false) grid start goal_pos =
    let dist_from_start = dist_from_start grid start goal_pos in
    let dist_mat = dists grid goal_pos in
    List.map
      (fun e ->
        calc_dist dist_mat 0 e
        + dist_from_start.(List.hd e)
        + if go_back then dist_from_start.(List.rev e |> List.hd) else 0)
      (Lib.permutations (List.init (List.length goal_pos) Lib.id))
    |> List.fold_left min max_int

  let main ?(go_back = false) () =
    let goal_pos, start = get_goal_pos_list cnt in
    find_shortest_path ~go_back cnt start goal_pos
end

let part1 () = P1.main () |> Lib.print_int
let part2 () = P1.main ~go_back:true () |> Lib.print_int
