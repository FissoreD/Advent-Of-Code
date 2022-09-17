module T = struct
  type t = { x : int; y : int }

  let compare p1 p2 = compare (p1.y, p1.x) (p2.y, p2.x)
end

module Dir = struct
  type dirs = E | W | N | S | NE | NW | SE | SW
  type dir_t = D4 | D6 | D8
  type grid_t = Square | Hexagon

  let list_d4 = [ N; S; W; E ]
  let list_d6 = [ N; S; NE; NW; SE; SW ]
  let list_d8 = E :: W :: list_d6
end

open Dir
include T

let str_to_dir = function
  | "s" -> S
  | "n" -> N
  | "ne" -> NE
  | "se" -> SE
  | "nw" -> NW
  | "sw" -> SW
  | "w" -> W
  | "e" -> E
  | _ -> failwith "Error in Pos"

let move_in_square { x; y } = function
  | N -> { x; y = y - 1 }
  | S -> { x; y = y + 1 }
  | W -> { x = x - 1; y }
  | E -> { x = x + 1; y }
  | SE -> { x = x + 1; y = y + 1 }
  | SW -> { x = x - 1; y = y + 1 }
  | NE -> { x = x + 1; y = y - 1 }
  | NW -> { x = x - 1; y = y - 1 }

let move_in_hexagon { x; y } = function
  | N -> { x; y = y - 2 }
  | S -> { x; y = y + 2 }
  | (SE | SW | NE | NW) as dir -> move_in_square { x; y } dir
  | _ -> failwith "In exagon error"

let dist_pythagoras a b =
  sqrt ((float (abs (a.x - b.x)) ** 2.) +. (float (abs (a.y - b.y)) ** 2.))

let zero = { x = 0; y = 0 }
let max_pos = { x = max_int; y = max_int }
let add a b = { x = a.x + b.x; y = a.y + b.y }
let sub a b = { x = a.x - b.x; y = a.y - b.y }
let is_positive { x; y } = x >= 0 && y >= 0
let is_valid (maxX, maxY) p = is_positive p && p.x < maxX && p.y < maxY
let give_dir_list = function D4 -> list_d4 | D6 -> list_d6 | D8 -> list_d8

let neighbors ?(dir_t = D4) ?dirs pos =
  List.map (move_in_square pos)
    (Option.value ~default:(give_dir_list dir_t) dirs)

let positive_neighbors pos = neighbors pos |> List.filter is_positive
let to_string { x; y } = Printf.sprintf "{x: %d; y=%d}" x y

let print ?br pos =
  print_string @@ to_string pos;
  if br <> None then print_endline ""

let find_nearest ?(dir_t = D4) grid (s : t) (e : t list) : t list * int =
  let module Set = Set.Make (T) in
  let w, h = (Array.length grid.(0), Array.length grid) in
  let explored = Hashtbl.create (w * h) in
  let e = Set.of_list e in
  let is_valid pos =
    (not @@ Hashtbl.mem explored pos)
    && is_valid (w, h) pos
    && grid.(pos.y).(pos.x) <> None
  in
  let rec aux pos_l dist =
    match Set.inter e pos_l with
    | x when x = Set.empty ->
        let next =
          Set.fold
            (fun e acc ->
              if is_valid e then (
                Hashtbl.add explored e 0;
                List.fold_left
                  (fun acc e -> if is_valid e then Set.add e acc else acc)
                  acc (neighbors ~dir_t e))
              else acc)
            pos_l Set.empty
        in
        if next <> Set.empty then aux next (dist + 1) else ([], -1)
    | inter -> (Set.elements inter, dist)
  in
  aux (Set.singleton s) 0

(** returns the shortest path from a position [s] to a position [e], from a grid where [None] cells represents walls  *)
let shortest_path_len ?(dir_t = D4) grid s e =
  snd (find_nearest ~dir_t grid s [ e ])

(** returns the "air" distance between two positions, it is like an infinite grid without walls *)
let air_distance dir_t grid_t s e =
  match (dir_t, grid_t) with
  | D4, Square -> abs (s.x - e.x) + abs (s.y - e.y)
  | D8, Square ->
      let a, b = (abs (s.x - e.x), abs (s.y - e.y)) in
      (a lsr 1) + (b lsr 1) + (a land 1) + (b land 1)
  | D6, Hexagon -> (abs (s.x - e.x) + abs (s.y - e.y)) / 2
  | _ -> failwith "Shortest air path not implemented"
