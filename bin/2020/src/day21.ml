open Lib
module Set' = Set.Make (String)
module Map' = Map.Make (String)

let print_map m =
  Map'.iter
    (fun e e' ->
      let l = Set'.fold List.cons e' [] in
      Printf.printf "%s -> " e;
      List.print id l;
      print_newline ())
    m

let parse_line l =
  let l' = String.split_on_char '(' l in
  let fst = List.hd l' |> String.trim |> String.split_on_char ' ' in
  let snd = String.split_on_char ',' (List.get 1 l') |> List.map String.trim in
  let len = List.length snd >= 2 in
  let snd' = if len then List.tl snd |> List.rev |> List.tl else [] in
  let hd = List.hd snd |> String.split_on_char ' ' |> List.get 1 in
  let tl =
    (if len then List.rev snd |> List.hd else hd)
    |> String.split_on_char ')' |> List.hd
  in
  (fst, if len then hd :: tl :: snd' else [ tl ])

let inp () = read_file "day21" parse_line

let create_set i = Set'.of_list (List.map snd i |> List.flatten)

let create_map s (l : (string list * string list) list) =
  let exist s (_, e) = List.exists (( = ) s) e in
  Set'.fold
    (fun s elt ->
      let res =
        List.filter (exist s) l |> List.map (fun (a, _) -> Set'.of_list a)
      in
      let res' = List.fold_left Set'.inter (List.hd res) res in
      Map'.add s res' elt)
    s Map'.empty

let day21_1 () =
  let i = inp () in
  let all_mistery = List.map fst i |> List.map Set'.of_list in
  let all_food = create_set i in
  let map = create_map all_food i in
  List.map
    (fun e ->
      Set'.diff e
        (Map'.bindings map |> List.map snd
        |> List.fold_left Set'.union Set'.empty))
    all_mistery
  |> List.map Set'.cardinal |> List.fold_left ( + ) 0

let rec assoc_elts valid map =
  let f valid' =
    List.map (fun (a, b) -> (a, Set'.choose b)) @@ Map'.bindings valid'
  in
  if Map'.cardinal map = 0 then valid
  else
    let valid', invalid = Map'.partition (fun _ c -> Set'.cardinal c = 1) map in
    let valid'' =
      Map'.fold
        (fun _ e elt -> Set'.union e elt)
        valid'
        (Map'.choose valid' |> snd)
    in
    let invalid = Map'.map (fun s -> Set'.diff s valid'') invalid in
    assoc_elts (f valid' @ valid) invalid

let rec cat = function [] -> "" | [ a ] -> a | hd :: tl -> hd ^ "," ^ cat tl

let day21_2 () =
  let i = inp () in
  let all_food = create_set i in
  let map = create_map all_food i in
  map |> assoc_elts []
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map snd |> cat
