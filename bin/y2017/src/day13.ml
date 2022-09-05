(* https://adventofcode.com/2017/day/13 *)

let cnt () =
  let parse_line e =
    let res = Re.Str.(split (regexp ": ") e |> List.map int_of_string) in
    match res with a :: b :: _ -> (a, b) | _ -> invalid_arg "Error"
  in
  Lib.read_file "17" "13" parse_line

module P1 = struct
  let position (depth, range) =
    let r = depth mod ((range - 1) lsl 1) in
    if r < range then r else ((range - 1) lsl 1) - r

  let main cnt =
    let open List in
    let snd_if_zero (x, y) = if x = 0 then Some y else None in
    let position (d, r) = (position (d, r), d * r) in
    map position cnt |> filter_map snd_if_zero |> List.fold_left ( + ) 0
end

module P2 = struct
  let main delay cnt =
    let open List in
    let position (d, r) = P1.position (d + delay, r) <> 0 in
    for_all position cnt

  let main cnt =
    let rec find_zero delay =
      if main delay cnt then delay else find_zero (delay + 1)
    in
    find_zero 0
end

let part1 () = P1.main (cnt ()) |> Lib.print_int
let part2 () = P2.main (cnt ()) |> Lib.print_int
