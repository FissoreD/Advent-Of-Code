(* https://adventofcode.com/2017/day/11 *)

let cnt () = Lib.read_file "17" "11" (String.split_on_char ',') |> List.hd

module P1 = struct
  let rec move pos dir =
    match dir with
    | [] -> pos
    | hd :: tl -> move (Pos.move_in_hexagon pos (Pos.str_to_dir hd)) tl

  let main cnt = move Pos.zero cnt |> Pos.air_distance D6 Hexagon Pos.zero
end

module P2 = struct
  let rec move acc pos dir =
    match dir with
    | [] -> acc
    | hd :: tl ->
        let pos = Pos.move_in_hexagon pos (Pos.str_to_dir hd) in
        move (max (Pos.air_distance D6 Hexagon pos Pos.zero) acc) pos tl

  let main cnt = move 0 Pos.zero cnt
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
