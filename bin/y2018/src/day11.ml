(* https://adventofcode.com/2018/day/11 *)

let cnt () = Lib.read_file "18" "11" int_of_string |> List.hd

module P1 = struct
  let give_value serial (p : Pos.t) =
    ((((p.x + 10) * p.y) + serial) * (p.x + 10) / 100 mod 10) - 5

  let square_id_sum ?(len = 3) (p : Pos.t) mat =
    let res = ref 0 in
    for x = 0 to len - 1 do
      for y = 0 to len - 1 do
        let p = Pos.add p { x; y } in
        let cell_weight =
          match mat.(p.y).(p.x) with exception _ -> 0 | n -> n
        in
        res := !res + cell_weight
      done
    done;
    !res

  let build_mat ?(size = 300) cnt =
    let mat = Array.make_matrix (size + 1) (size + 1) 0 in
    for x = 1 to size do
      for y = 1 to size do
        mat.(y).(x) <- give_value cnt { x; y }
      done
    done;
    mat

  let main cnt =
    let mat = build_mat cnt in
    let rec aux (((_ : Pos.t), sum) as res) = function
      | 298, 297 -> res
      | 298, y -> aux res (1, y + 1)
      | x, y ->
          let sum' = square_id_sum { x; y } mat in
          aux (if sum' > sum then ({ x; y }, sum') else res) (x + 1, y)
    in
    let ({ x; y } : Pos.t), _ = aux (Pos.zero, 0) (1, 1) in
    Printf.sprintf "%d,%d" x y
end

module P2 = struct
  include P1

  let best_from_block_size size mat bs (* block size *) =
    let chunks =
      Array.init size (fun x ->
          let weight = ref 0 in
          for y = 0 to bs - 1 do
            weight := !weight + mat.(y).(x)
          done;
          !weight)
    in
    let best = ref (Pos.zero, 0) in
    for y = 0 to size - bs do
      let total = ref (Array.sub chunks 0 bs |> Array.fold_left ( + ) 0) in
      for x = 0 to size - bs do
        if !total > snd !best then best := ({ x; y }, !total);
        if x < size - bs then total := !total + chunks.(x + bs) - chunks.(x)
      done;
      if y < size - bs then
        for x = 0 to size - bs do
          chunks.(x) <- chunks.(x) + mat.(y + bs).(x) - mat.(y).(x)
        done
    done;
    !best

  let main ~size cnt =
    let mat = build_mat ~size cnt in
    let best = ref (Pos.zero, min_int) in
    let old_weight = ref 0 in
    for block_size = 1 to size do
      let pos, weight = best_from_block_size size mat block_size in
      if weight > !old_weight then (
        best := (pos, block_size);
        old_weight := weight)
    done;
    let pos, size = !best in
    Printf.sprintf "%d,%d,%d" pos.x pos.y size
end

let part1 () = P1.main (cnt ())
let part2 () = P2.main ~size:300 (cnt ())
