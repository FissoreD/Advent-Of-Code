open Lib

let parse_line l =
  let open List in
  let l' = String.split_on_char ' ' l in
  let bounds, letter, mot =
    (hd l', String.get (tl l' |> hd) 0, hd (tl (tl l')))
  in
  let bounds =
    let l = String.split_on_char '-' bounds |> List.map int_of_string in
    (hd l, hd (tl l))
  in
  (bounds, letter, mot)

let valid_line1 l =
  let bounds, letter, mot = parse_line l in
  if in_bouond_inclusive bounds (List.count letter (string_2_char_list mot))
  then 1
  else 0

let inp1 () = read_file "day02" valid_line1

let day02_1 () = inp1 () |> List.fold_left ( + ) 0

let valid_line2 l =
  let (a, b), letter, mot = parse_line l in
  xor (mot.[a - 1] = letter) (mot.[b - 1] = letter) |> bool_to_int

let day02_2 () = read_file "day02" valid_line2 |> List.fold_left ( + ) 0
