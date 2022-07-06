open Lib

let create_cond l =
  let open List in
  let l = String.split_on_char ':' l in
  let name = hd l in
  let tail = tl l |> hd |> String.trim |> String.split_on_char ' ' in
  let b1, b2 = (hd tail, tl tail |> tl |> hd) in
  let create_bound b =
    let res = String.split_on_char '-' b |> List.map int_of_string in
    (hd res, hd (tl res))
  in
  (name, create_bound b1, create_bound b2)

let parse_line l = String.split_on_char ',' l |> List.map int_of_string

let parse_inp l =
  let rec first_lines acc = function
    | hd :: tl when hd <> "" -> first_lines (create_cond hd :: acc) tl
    | x -> (acc, List.(tl (tl x)))
  in
  first_lines [] l

let inp () =
  let open List in
  let f = read_file "day16" (fun i -> i) in
  let conds, tail = parse_inp f in
  let my_ticket = parse_line (hd tail) in
  let their_ticket = List.map parse_line (tail |> tl |> tl |> tl) in
  (conds, my_ticket, their_ticket)

let check_cond b1 b2 cond =
  in_bouond_inclusive b1 cond || in_bouond_inclusive b2 cond

let rec valid_line conds = function
  | [] -> 0
  | cond :: tl ->
      let f (_, c1, c2) = check_cond c1 c2 cond in
      (if List.exists f conds then 0 else cond) + valid_line conds tl

let day16_1 () =
  let conds, _, their = inp () in
  List.fold_left (fun acc elt -> acc + valid_line conds elt) 0 their

type t16 = (string * (int * int) * (int * int)) list

let rec create_cols_one =
  List.map2 (fun provv' elt ->
      List.filter (fun (name, b1, b2) -> check_cond b1 b2 elt) provv')

let filter_all =
  let rec aux finish_list cols_filtered' =
    let ok, ko =
      List.partition (fun (pos, elt) -> List.tl elt = []) cols_filtered'
    in
    let ok = List.map (fun (pos, l) -> (pos, List.hd l)) ok in
    let ko =
      List.map
        (fun (pos, l) ->
          ( pos,
            List.filter (fun elt -> List.exists (fun (_, x) -> x <> elt) ok) l
          ))
        ko
    in
    if ko = [] then
      ok @ finish_list |> List.map (fun (pos, (name, _, _)) -> (pos, name))
    else aux (finish_list @ ok) ko
  in
  aux []

let day16_2 () =
  let conds, my, theirs = inp () in
  let theirs = List.filter (fun elt -> valid_line conds elt = 0) theirs in
  let columns = List.(init (length (hd theirs)) (fun _ -> conds)) in
  let cols_filterd = create_cols_one columns (List.hd theirs) in
  let cols_filterd' =
    List.fold_left (fun acc elt -> create_cols_one acc elt) cols_filterd theirs
  in
  let f acc value (pos, name) =
    let s = String.split_on_char ' ' name |> List.hd = "departure" in
    acc * if s then value else 1
  in
  cols_filterd'
  |> List.mapi (fun pos elt -> (pos, elt))
  |> filter_all
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.fold_left2 f 1 my
