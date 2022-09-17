(* https://adventofcode.com/2017/day/3 *)

let cnt () = Lib.read_file "17" "03" int_of_string |> List.hd

module P1 = struct
  let crown_nb n = int_of_float (ceil (sqrt (float n)) /. 2.)

  let dist n =
    let crown_nb = crown_nb n in
    let max_nb = (Lib.double crown_nb + 1) * (Lib.double crown_nb + 1) in
    let rec aux = function
      | 4 -> max_int
      | i -> min (aux (i + 1)) (abs (max_nb - (crown_nb * ((2 * i) + 1)) - n))
    in
    crown_nb + aux 0

  let main = dist
end

module P2 = struct
  let build_map stop =
    let tbl = Hashtbl.create 256 in
    let neigh_value p =
      Pos.neighbors ~dir_t:D8 p
      |> List.filter_map (Hashtbl.find_opt tbl)
      |> List.fold_left ( + ) 0
    in
    let next_dir = Pos.Dir.(function N -> W | W -> S | S -> E | _ -> N) in
    let next_pos ({ x; y } as p : Pos.t) dir =
      let is_next_dir (res : Pos.t) =
        res.x = -res.y || (res.x > 0 && x = y) || (res.x < 0 && res.x = res.y)
      in
      let res = Pos.move_in_square p dir in
      (res, if is_next_dir res then next_dir dir else dir)
    in
    let rec add_next (p, dir) =
      let value = neigh_value p in
      Hashtbl.add tbl p value;
      if value > stop then value else add_next (next_pos p dir)
    in
    Hashtbl.add tbl Pos.zero 1;
    add_next ({ x = 1; y = 0 }, N)

  let main cnt = build_map cnt
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
