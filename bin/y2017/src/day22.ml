(* https://adventofcode.com/2017/day/22 *)

let cnt () = Lib.read_file "17" "22" Lib.string_2_char_list

module P1 = struct
  type d = Pos.T.t

  let find tbl p = match Hashtbl.find_opt tbl p with None -> '.' | Some a -> a

  let swap_cnt tbl p =
    Hashtbl.replace tbl p (match find tbl p with '.' -> '#' | _ -> '.')

  let rotate_right : d -> d = function N -> E | E -> S | S -> W | _ -> N
  let rotate_left : d -> d = function N -> W | W -> S | S -> E | _ -> N

  let main ?(times = 10_000) cnt =
    let (tbl : (Pos.t, char) Hashtbl.t) = Hashtbl.create (1 lsr 15) in
    let w, h = List.(length (hd cnt), length cnt) in
    List.iteri
      (fun y e ->
        List.iteri (fun x e -> if e = '#' then Hashtbl.add tbl { x; y } e) e)
      cnt;
    let rec aux dir pos burst acc =
      if acc = times then burst
      else
        let old_cnt = find tbl pos in
        let new_dir =
          (if old_cnt = '#' then rotate_right else rotate_left) dir
        in
        let burst = burst + Lib.bool_to_int (old_cnt = '.') in
        swap_cnt tbl pos;
        aux new_dir (Pos.move_in_square pos new_dir) burst (acc + 1)
    in
    aux N { x = w / 2; y = h / 2 } 0 0
end

module P2 = struct
  include P1

  let node_change = function '.' -> 'w' | 'w' -> '#' | '#' -> 'f' | _ -> '.'

  let rotation_rule = function
    | '.' -> rotate_left
    | 'w' -> Fun.id
    | '#' -> rotate_right
    | _ -> fun e -> rotate_left (rotate_left e)

  let main ?(times = 10_000_000) cnt =
    let (tbl : (Pos.t, char) Hashtbl.t) = Hashtbl.create (1 lsr 15) in
    let w, h = List.(length (hd cnt), length cnt) in
    List.iteri
      (fun y e ->
        List.iteri (fun x e -> if e = '#' then Hashtbl.add tbl { x; y } e) e)
      cnt;
    let rec aux dir pos burst acc =
      if acc = times then burst
      else
        let old_cnt = find tbl pos in
        let new_dir = (rotation_rule old_cnt) dir in
        let burst = burst + Lib.bool_to_int (old_cnt = 'w') in
        Hashtbl.replace tbl pos (node_change (find tbl pos));
        aux new_dir (Pos.move_in_square pos new_dir) burst (acc + 1)
    in
    aux N { x = w / 2; y = h / 2 } 0 0
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
