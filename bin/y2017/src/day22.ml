(* https://adventofcode.com/2017/day/22 *)

let cnt () = Lib.read_file "17" "22" Lib.string_2_char_list

module P1 = struct
  type d = Pos.T.t

  let find tbl p = match Hashtbl.find_opt tbl p with None -> '.' | Some a -> a
  let replace tbl k v = Hashtbl.replace tbl k v
  let rotate_right : d -> d = function N -> E | E -> S | S -> W | _ -> N
  let rotate_left : d -> d = function N -> W | W -> S | S -> E | _ -> N
  let node_change = function '.' -> '#' | _ -> '.'

  let rotation_rule = function
    | '.' -> rotate_left
    | '#' -> rotate_right
    | 'w' -> Fun.id
    | _ -> fun e -> rotate_left (rotate_left e)

  let main ?(times = 10_000) ?(node_change = node_change) cnt =
    let (tbl : (Pos.t, char) Hashtbl.t) = Hashtbl.create (1 lsl 22) in
    let w, h = List.(length (hd cnt), length cnt) in
    List.iteri (fun y e -> List.iteri (fun x e -> replace tbl { x; y } e) e) cnt;
    let rec aux dir pos burst = function
      | n when n = times -> burst
      | n ->
          let old_cnt = find tbl pos in
          let dir = (rotation_rule old_cnt) dir in
          let new_char = node_change old_cnt in
          let burst = burst + Lib.bool_to_int (new_char = '#') in
          replace tbl pos new_char;
          aux dir (Pos.move_in_square pos dir) burst (n + 1)
    in
    aux N { x = w / 2; y = h / 2 } 0 0
end

module P2 = struct
  include P1

  let node_change = function '.' -> 'w' | 'w' -> '#' | '#' -> 'f' | _ -> '.'
  let main ?(times = 10_000_000) = main ~times ~node_change
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
