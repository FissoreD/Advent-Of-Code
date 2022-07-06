open Lib

let split_inp text =
  let calc_id text =
    String.split_on_char ' ' text
    |> List.tl |> List.hd |> String.split_on_char ':' |> List.hd
    |> int_of_string
  in
  let rec aux id l = function
    | "" :: hd :: tl -> (id, l) :: aux (calc_id hd) [] tl
    | hd :: tl -> aux id (string_2_char_list hd :: l) tl
    | [] -> [ (id, l) ]
  in
  aux (List.hd text |> calc_id) [] (List.tl text)

let inp () =
  let txt = read_file "day20" id in
  split_inp txt

type 'a neighT = { up : 'a; left : 'a; right : 'a; down : 'a }

let all_sides tile =
  let open List in
  let trans = transpose_matrix tile in
  {
    up = hd tile;
    down = hd (rev tile);
    left = hd trans;
    right = hd (rev trans);
  }

let find_neigh tile_list (num, tile) =
  let open List in
  let { up; down; left; right } = all_sides tile in
  let tile_list = remove_assoc num tile_list in
  let f side =
    List.filter
      (fun (num, t) ->
        let { up = up'; down = down'; left = left'; right = right' } =
          all_sides t
        in
        let sides' = [ up'; down'; left'; right' ] in
        exists (fun side' -> side' = side || side' = rev side) sides')
      tile_list
    |> List.map fst
  in
  (num, { up = f up; down = f down; left = f left; right = f right })

let corners =
  List.filter (fun (num, { up; down; left; right }) ->
      (up = [] && left = [])
      || (up = [] && right = [])
      || (down = [] && right = [])
      || (down = [] && left = []))

let day20_1 () =
  let neighs i = List.map (find_neigh i) i in
  inp () |> neighs |> corners |> List.map fst |> List.fold_left ( * ) 1

type 'a type20 = { id : int; mat : char list list; neighs : 'a neighT }

type sides = North | South | East | West

let all_comb_mat (m : char list list) =
  let open List in
  let mrev = map rev m in
  let mT = transpose_matrix m in
  let mTrev = map rev mT in
  [ m; rev m; mT; rev mT; mrev; rev mrev; mTrev; rev mTrev ]

let opposite sides = function
  | North -> sides.down
  | South -> sides.up
  | East -> sides.left
  | West -> sides.right

let find_neigh mat_list (id, mat) =
  let open List in
  let all_comb_mat (id, mat) =
    all_comb_mat mat |> map (fun mat -> { id; mat; neighs = all_sides mat })
  in
  let rec find_first side dir = function
    | [] -> []
    | { id; mat; neighs } :: _ when side = opposite neighs dir -> [ (id, mat) ]
    | _ :: tl -> find_first side dir tl
  in
  let sides = all_sides mat in
  let acc = { up = []; down = []; left = []; right = [] } in
  let rec aux acc = function
    | [] -> acc
    | mat :: tl ->
        if fst mat <> id then
          let all_comb = all_comb_mat mat in
          let acc =
            {
              up =
                (if acc.up <> [] then acc.up
                else find_first sides.up North all_comb);
              down =
                (if acc.down <> [] then acc.down
                else find_first sides.down South all_comb);
              right =
                (if acc.right <> [] then acc.right
                else find_first sides.right East all_comb);
              left =
                (if acc.left <> [] then acc.left
                else find_first sides.left West all_comb);
            }
          in
          aux acc tl
        else aux acc tl
  in
  { id; mat; neighs = aux acc mat_list }

let rotateM m =
  let aux m =
    List.init
      (List.length (List.hd m))
      (fun x ->
        List.init (List.length m) (fun y ->
            List.nth (List.nth m (List.length m - y - 1)) x))
  in
  let up, down, left, right =
    let f = function [] -> [] | (a, b) :: _ -> [ (a, aux b) ] in
    (f m.neighs.left, f m.neighs.right, f m.neighs.down, f m.neighs.up)
  in
  { m with mat = aux m.mat; neighs = { up; down; left; right } }

let rec myFind id' = function
  | [] -> raise Invalid_input
  | ({ id } as hd) :: _ when id = id' -> hd
  | _ :: tl -> myFind id' tl

let merge m l side =
  let act f = function [] -> [] | (a, b) :: _ -> [ (a, f b) ] in
  let rec aux current isUp =
    match if isUp then current.neighs.down else current.neighs.right with
    | (id', mat') :: _ when id' = m.id ->
        if m.mat = mat' then current
        else
          let f1 = if isUp then id else act (List.map List.rev) in
          let f2 = if not isUp then id else act List.rev in
          let up = f1 current.neighs.up in
          let down = f1 current.neighs.down in
          let left = f2 current.neighs.left in
          let right = f2 current.neighs.right in
          {
            id = current.id;
            mat = (current.mat |> if isUp then List.map List.rev else List.rev);
            neighs =
              {
                up = (if not isUp then down else up);
                down = (if not isUp then up else down);
                left = (if not isUp then left else right);
                right = (if not isUp then right else up);
              };
          }
    | _ -> aux (rotateM current) isUp
  in
  let find_next_m b = function
    | [] -> []
    | i :: _ -> [ aux (myFind (fst i) l) b ]
  in
  match side with
  | West -> find_next_m false m.neighs.left
  | North -> find_next_m true m.neighs.up
  | _ -> raise Invalid_input

let rec create_line m l =
  match merge m l West with [] -> [ m ] | hd :: _ -> m :: create_line hd l

let rec create_column m l =
  match merge m l North with [] -> [ m ] | hd :: _ -> m :: create_column hd l

let rec rotate_corner ({ neighs } as hd) =
  let rec aux m =
    if m.neighs.up <> [] && m.neighs.left <> [] then m else aux (rotateM m)
  in
  aux hd

let corners' =
  List.filter (fun { neighs } ->
      let { up; down; left; right } = neighs in
      (up = [] && left = [])
      || (up = [] && right = [])
      || (down = [] && right = [])
      || (down = [] && left = []))

let make_full_mat m =
  let c = List.hd (corners' m) |> rotate_corner in
  let col = create_column c m in
  List.map (fun c -> create_line c m) col |> List.map List.rev |> List.rev

let create_matrix m =
  let m' = List.map (List.map (fun { mat } -> mat)) m in
  m'

let simplify ({ id; neighs } : (int * char list list) list type20) =
  let separe = function [] -> [] | (id, _) :: _ -> [ id ] in
  let { up; left; right; down } = neighs in
  {
    id;
    mat = [];
    neighs =
      {
        up = separe up;
        left = separe left;
        right = separe right;
        down = separe down;
      };
  }

let merge_mats ml =
  let rec aux i = function [] -> [] | hd :: tl -> List.get i hd @ aux i tl in
  let rec aux' m = function
    | n when n = List.length (List.hd m) -> []
    | n -> aux n m :: aux' m (n + 1)
  in
  List.map (fun p -> aux' p 0) ml

let remove_sides m =
  let remove_up_down m = List.rev (List.tl (List.rev (List.tl m))) in
  List.map remove_up_down m |> remove_up_down

let dragon =
  List.map string_2_char_list
    [ "                  # "; "#    ##    ##    ###"; " #  #  #  #  #  #   " ]
  |> List.mapi (fun i e ->
         e |> List.mapi (fun i' e' -> if e' = '#' then (i, i') else (-1, -1)))
  |> List.flatten
  |> List.filter (fun e -> e <> (-1, -1))

let find_dragon m =
  let open List in
  let mat_height = length m - 3 in
  let mat_width = length (hd m) - 20 in
  let get m (a, b) = get a m |> get b in
  let mats' = all_comb_mat m in
  let aux m c =
    List.for_all (fun c' -> get m (c ++ c') = '#') dragon |> bool_to_int
  in
  let rec all_mats m c =
    match c with
    | x, y when x = mat_height -> 0
    | x, y when y = mat_width -> all_mats m (x + 1, 0)
    | c -> (try aux m c with _ -> 0) + all_mats m (c ++ (0, 1))
  in
  let rec aux' = function
    | [] -> raise Invalid_input
    | hd :: tl -> ( match all_mats hd (0, 0) with 0 -> aux' tl | n -> n)
  in
  aux' mats'

let day20_2 () =
  let i = inp () in
  let m =
    List.map (find_neigh i) i
    |> make_full_mat |> create_matrix
    |> List.map (List.map remove_sides)
    |> merge_mats
    |> List.fold_left (fun acc elt -> acc @ elt) []
  in
  (List.flatten m |> List.count '#') - (find_dragon m * List.length dragon)

let _ = print_int @@ day20_2 ()
