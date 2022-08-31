type t = { x : int; y : int }
type dir = U | D | L | R

let dir_list = [ U; D; L; R ]

let move_from_dir { x; y } = function
  | U -> { x; y = y - 1 }
  | D -> { x; y = y + 1 }
  | L -> { x = x - 1; y }
  | R -> { x = x + 1; y }

let move_from_pos a b = { x = a.x + b.x; y = a.y + b.y }
let is_positive_pos { x; y } = x >= 0 && y >= 0
let is_valid_pos (maxX, maxY) p = is_positive_pos p && p.x < maxX && p.y < maxY
let neighbors pos = List.map (move_from_dir pos) dir_list
let positive_neighbors pos = neighbors pos |> List.filter is_positive_pos
let pos_to_string { x; y } = Printf.sprintf "{x: %d; y=%d}" x y
let print_pos pos = print_string @@ pos_to_string pos

let shortest_path_len (grid : 'a option array array) (s : t) (e : t) =
  let (q : (t * int) Queue.t) = Queue.create () in
  let explored : t list ref = ref [] in
  let current = ref (s, 0) in
  let w, h = (Array.length grid.(0), Array.length grid) in
  Queue.add !current q;
  while fst !current <> e do
    if Queue.is_empty q then invalid_arg "Not such path";
    current := Queue.pop q;
    if
      is_valid_pos (w, h) (fst !current)
      && (not @@ List.mem (fst !current) !explored)
      && grid.((fst !current).y).((fst !current).x) <> None
    then (
      explored := fst !current :: !explored;
      let neigs = neighbors @@ fst !current in
      List.iter (fun e -> Queue.add (e, snd !current + 1) q) neigs)
  done;
  snd !current
