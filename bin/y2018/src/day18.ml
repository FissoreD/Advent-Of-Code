(* https://adventofcode.com/2018/day/18 *)

let cnt () =
  Lib.read_file "18" "18" (fun e -> Lib.string_2_char_list e |> Array.of_list)
  |> Array.of_list

module P1 = struct
  let add_couple ((t, l) as acc) = function
    | '|' -> (t + 1, l)
    | '#' -> (t, l + 1)
    | _ -> acc

  let count grid pos w h =
    let open Pos in
    let neighs = neighbors ~dir_t:D8 pos |> List.filter (is_valid (w, h)) in
    List.fold_left (fun c p -> add_couple c grid.(p.y).(p.x)) (0, 0) neighs

  let mutate m1 m2 w h =
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let t, l = count m1 { x; y } w h in
        match m1.(y).(x) with
        | '.' when t > 2 -> m2.(y).(x) <- '|'
        | '|' when l > 2 -> m2.(y).(x) <- '#'
        | '#' when t = 0 || l = 0 -> m2.(y).(x) <- '.'
        | c -> m2.(y).(x) <- c
      done
    done;
    (m1, m2)

  let build_res m1 =
    let open Array in
    let multiply_couple (a, b) = a * b in
    fold_left (fold_left add_couple) (0, 0) m1 |> multiply_couple

  let main ~minutes cnt =
    let w, h = Array.(length cnt.(0), length cnt) in
    let memory = Hashtbl.create (2 lsl 22) in
    let rec repeat (m1, m2) = function
      | n when n = minutes -> build_res m1
      | n -> (
          let m1, m2 = mutate m1 m2 w h in
          let r1, r2 = (build_res m1, build_res m2) in
          match Hashtbl.find_opt memory r1 with
          | Some (r2', n') when r2 = r2' ->
              let rec aux c = function
                | 0 -> c
                | n -> aux (Hashtbl.find memory c |> fst) (n - 1)
              in
              aux r1 ((minutes - n) mod (n - n'))
          | _ ->
              Hashtbl.replace memory r1 (r2, n);
              repeat (m2, m1) (n + 1))
    in
    repeat (cnt, Array.(map (map Fun.id)) cnt) 0
end

let part1 () = P1.main ~minutes:10 (cnt ()) |> string_of_int
let part2 () = P1.main ~minutes:1_000_000_000 (cnt ()) |> string_of_int
