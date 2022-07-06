open Lib

let inp () = read_file "day01" int_of_string

let sum_to_n n l =
  let rec aux l l' =
    match (l, l') with
    | [], _ -> raise Invalid_input
    | _ :: tl, [] -> aux tl (List.tl tl)
    | hd :: tl, hd' :: tl' when hd + hd' = n -> hd * hd'
    | l, hd' :: tl' -> aux l tl'
  in
  aux l (List.tl l)

let day01_1 () = inp () |> sum_to_n 2020

let sum_to_n' n l =
  let l = List.sort compare l |> List.rev in
  let open List in
  let rec aux l l' l'' =
    match (l, l', l'') with
    | hd :: _, hd' :: hd'' :: tl, _ when hd + hd' > n -> aux l (hd'' :: tl) tl
    | hd :: _, hd' :: _, hd'' :: _ when hd + hd' + hd'' = n -> hd * hd' * hd''
    | [], _, _ -> raise Invalid_input
    | _ :: tl, [], _ -> aux tl (List.tl tl) (List.tl (List.tl tl))
    | _ :: tl, [ _ ], [] -> aux tl (List.tl tl) (List.tl (List.tl tl))
    | _, _ :: hd :: tl, [] -> aux l (hd :: tl) tl
    | _ :: _, _ :: _, _ :: tl -> aux l l'' tl
  in
  aux l (List.tl l) (List.tl (List.tl l))

let day01_2 () = inp () |> sum_to_n' 2020
