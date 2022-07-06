open Lib

type type19 = Nb of int | Lt of string

let parse_line l =
  let open String in
  let open List in
  let l = split_on_char ':' l in
  let name = hd l |> int_of_string in
  let rules =
    hd (tl l)
    |> split_on_char '|'
    |> map (fun e ->
           trim e |> split_on_char ' '
           |> map (fun e ->
                  if is_int e then Nb (int_of_string e)
                  else Lt (char_of_string e.[1])))
  in
  (name, rules)

let inp () =
  let file = openF "day19" in
  let rec aux () =
    let line = input_line file in
    if line = "" then [] else parse_line line :: aux ()
  in
  let rec entries () =
    try
      let line = input_line file in
      line :: entries ()
    with End_of_file -> []
  in
  let a = aux () in
  let b = entries () in
  closeF file;
  (a, b)

let rec create_regex n conds =
  let open List in
  let back = "\\" in
  let openP, closeP, orP = (back ^ "(", back ^ ")", back ^ "|") in
  let cond elt str = if elt <> "" then str else "" in
  let base_case = function Nb n -> create_regex n conds | Lt l -> l in
  assoc n conds
  |> fold_left
       (fun acc' elt ->
         cond acc' openP ^ acc' ^ cond acc' orP
         ^ fold_left
             (fun acc elt' ->
               cond acc openP ^ acc ^ base_case elt' ^ cond acc closeP)
             "" elt
         ^ cond acc' closeP)
       ""

let add_start_end s = "^" ^ s ^ "$"

let day19_1 () =
  let a, b = inp () in
  let regex = Str.regexp (create_regex 0 a |> add_start_end) in
  List.map (fun e -> Str.string_match regex e 0 |> bool_to_int) b
  |> List.fold_left ( + ) 0

(* 
  8: 42 | 42 8
  11: 42 31 | 42 11 31 
*)
let matchNtimes a b =
  let rec aux acc times =
    if times = 15 then [] else acc :: aux (a ^ acc ^ b) (times + 1)
  in
  aux (a ^ b) 0

let day19_2 () =
  let open List in
  let a, b = inp () in
  let _42 = create_regex 42 a in
  let _31 = create_regex 31 a in
  let all_rules =
    matchNtimes _42 _31
    |> map (fun elt -> Str.regexp ("^\\(" ^ _42 ^ "+\\)\\(" ^ elt ^ "\\)$"))
  in
  map (fun s -> exists (fun exp -> Str.string_match exp s 0) all_rules) b
  |> count true

let _ = day19_2 () |> print_int
