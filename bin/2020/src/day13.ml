open Lib

(** return (start, (elt, pos) list)  *)
let inp () =
  let f = openF "day13" in
  let fst_line = input_line f |> int_of_string in
  let snd_line =
    input_line f |> String.split_on_char ','
    |> List.mapi (fun i elt ->
           ((if elt <> "x" then int_of_string elt else -1), i))
    |> List.filter (fun (a, b) -> a <> -1)
  in
  closeF f;
  (fst_line, snd_line)

let calc start l =
  let rec aux start' = function
    | [] -> aux (start' + 1) l
    | (a, _) :: _ when start' mod a = 0 -> (start' - start) * a
    | _ :: tl -> aux start' tl
  in
  aux start l

let day13_1 () =
  let a, b = inp () in
  calc a b

let cond_is_valid start =
  let rec aux invalid valid = function
    | [] -> (invalid, valid)
    | (elt, pos) :: tl when (start + pos) mod elt = 0 ->
        aux invalid (elt :: valid) tl
    | hd :: tl -> aux (hd :: invalid) valid tl
  in
  aux [] []

let calc' l =
  let max, index =
    List.fold_left
      (fun (elt, index) (elt', index') ->
        if elt > elt' then (elt, index) else (elt', index'))
      (0, 0) l
  in
  let l = List.filter (fun i -> fst i <> max) l in
  let rec aux step l current =
    let current' = current - index in
    let invalid, valid = cond_is_valid current' l in
    if invalid = [] then current'
    else
      let new_step = List.fold_left (fun acc elt -> acc * elt) step valid in
      aux new_step invalid (current + new_step)
  in
  aux max l max

let day13_2 () = inp () |> snd |> calc'
