open Lib

type pos = { line : int; raw : int }

type cell = B | W

type dir = E | SE | SW | W | NW | NE

type mat = cell ref list

let read_line l =
  let rec aux = function
    | [] -> []
    | 'e' :: tl -> E :: aux tl
    | 'w' :: tl -> W :: aux tl
    | 's' :: 'e' :: tl -> SE :: aux tl
    | 's' :: 'w' :: tl -> SW :: aux tl
    | 'n' :: 'e' :: tl -> NE :: aux tl
    | 'n' :: 'w' :: tl -> NW :: aux tl
    | _ -> raise Invalid_input
  in
  aux (string_2_char_list l)

let inp () = read_file "day24" read_line

let n = 80

let tot = ((n * 2) + 1) * ((n * 2) + 1)

let new_pos pos = function
  | E -> pos + 1
  | W -> pos - 1
  | SW -> pos + (n * 2)
  | SE -> pos + (n * 2) + 1
  | NW -> pos - (n * 2) - 1
  | NE -> pos - (n * 2)

let modify_cell (l : mat) pos =
  let x = List.get pos l in
  x := if !x = W then B else W

let create_mat () : mat = List.init tot (fun _ -> ref (W : cell))

let parse_line = List.fold_left new_pos

let day24_1 () =
  let inp = inp () in
  let start = ((n * 2) + 1) * ((n * 2) + 1) / 2 in
  let mat = create_mat () in
  List.iter
    (fun l ->
      let pos = parse_line start l in
      modify_cell mat pos)
    inp;
  List.count B (List.map ( ! ) mat)

type mat1 = cont list

and cont = { c : cell ref; n : cell ref list }

let modify_cell' (cell : cont) (cell' : cont) =
  let blacks = List.count B (cell.n |> List.map ( ! )) in
  cell'.c :=
    match !(cell.c) with
    | B when blacks = 0 || blacks > 2 -> W
    | W when blacks = 2 -> B
    | n -> n

let rec iter mat mat' = function
  | 0 -> mat
  | n ->
      print_int n;
      print_newline ();
      List.iter2 (fun c1 c2 -> modify_cell' c1 c2) mat mat';
      iter mat' mat (n - 1)

let create_mat1 (l : mat) : mat1 =
  let f elt = try List.nth l elt with _ -> ref (W : cell) in
  let neighs pos =
    [
      pos + 1;
      pos - 1;
      pos + (n * 2);
      pos + (n * 2) + 1;
      pos - (n * 2) - 1;
      pos - (n * 2);
    ]
  in
  List.mapi (fun pos elt -> { c = elt; n = (List.map f) (neighs pos) }) l

let day24_2 () =
  let inp = inp () in
  let start = tot / 2 in
  let mat = create_mat () in
  let mat1 : mat1 = create_mat1 mat in
  let mat2 : mat1 = List.map (fun i -> ref !i) mat |> create_mat1 in
  List.iter
    (fun l ->
      let pos = parse_line start l in
      modify_cell mat pos)
    inp;
  List.count B
    (List.map
       (fun { c } -> !c)
       (iter mat1 mat2 (Sys.argv.(1) |> int_of_string)))

(* let _ = day24_2 () |> print_int *)
