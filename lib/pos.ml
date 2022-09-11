module T = struct
  type t = E | W | N | S | NE | NW | SE | SW
  type dir_t = D4 | D6 | D8
  type grid_t = Square | Hexagon

  let list_d4 = [ N; S; W; E ]
  let list_d6 = [ N; S; NE; NW; SE; SW ]
  let list_d8 = E :: W :: list_d6
end

open T

type t = { x : int; y : int }

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
  | SE -> { x = x + 1; y = y + 1 }
  | SW -> { x = x - 1; y = y + 1 }
  | NE -> { x = x + 1; y = y - 1 }
  | NW -> { x = x - 1; y = y - 1 }
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

let neighbors ?(dir_t = D4) pos =
  List.map (move_in_square pos) (give_dir_list dir_t)

let positive_neighbors pos = neighbors pos |> List.filter is_positive
let to_string { x; y } = Printf.sprintf "{x: %d; y=%d}" x y
let print pos = print_string @@ to_string pos

(** returns the shortest path from a position [s] to a position [e], from a grid where [None] cells represents walls  *)
let shortest_path_len ?(dir_t = D4) grid s e =
  let module T = struct
    type t = { _x : int; _y : int }

    let compare = compare
  end in
  let module Set = Set.Make (T) in
  let w, h = (Array.length grid.(0), Array.length grid) in
  let to_T { x; y } : T.t = { _x = x; _y = y } in
  let explored = Hashtbl.create (w * h) in
  let (q : (t * int) Queue.t) = Queue.create () in
  let current = ref (s, 0) in
  let is_valid pos =
    (not @@ Hashtbl.mem explored (to_T pos))
    && is_valid (w, h) pos
    && grid.(pos.y).(pos.x) <> None
  in
  Queue.add !current q;
  while fst !current <> e do
    current := Queue.pop q;
    let pos, dist = !current in
    if is_valid pos then (
      Hashtbl.add explored (to_T pos) 0;
      List.iter (fun pos -> Queue.add (pos, dist + 1) q) (neighbors ~dir_t pos))
  done;
  snd !current

(** returns the "air" distance between two positions, it is like an infinite grid without walls *)
let air_distance dir_t grid_t s e =
  match (dir_t, grid_t) with
  | D4, Square -> abs (s.x - e.x) + abs (s.y - e.y)
  | D8, Square ->
      let a, b = (abs (s.x - e.x), abs (s.y - e.y)) in
      (a lsr 1) + (b lsr 1) + (a land 1) + (b land 1)
  | D6, Hexagon -> (abs (s.x - e.x) + abs (s.y - e.y)) / 2
  | _ -> failwith "Shortest air path not implemented"
