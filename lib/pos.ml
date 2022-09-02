type t = { x : int; y : int }
type dir = U | D | L | R

let dir_list = [ U; D; L; R ]

let move_from_dir { x; y } = function
  | U -> { x; y = y - 1 }
  | D -> { x; y = y + 1 }
  | L -> { x = x - 1; y }
  | R -> { x = x + 1; y }

let zero = { x = 0; y = 0 }
let move_from a b = { x = a.x + b.x; y = a.y + b.y }
let is_positive { x; y } = x >= 0 && y >= 0
let is_valid (maxX, maxY) p = is_positive p && p.x < maxX && p.y < maxY
let neighbors pos = List.map (move_from_dir pos) dir_list
let positive_neighbors pos = neighbors pos |> List.filter is_positive
let to_string { x; y } = Printf.sprintf "{x: %d; y=%d}" x y
let print pos = print_string @@ to_string pos

let shortest_path_len (grid : 'a option array array) (s : t) (e : t) =
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
      List.iter (fun pos -> Queue.add (pos, dist + 1) q) (neighbors pos))
  done;
  snd !current
