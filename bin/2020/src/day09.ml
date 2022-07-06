open Lib

let inp () = read_file "day09" int_of_string

(** Check if there is a couple of elt in l that adds up to n  *)
let calc_n_in_l n l =
  let l' = List.sort compare l in
  let rec aux l l' =
    match (l, l') with
    | [], _ | [ _ ], _ -> false
    | _ :: tl, [] -> aux tl (List.tl tl)
    | hd :: _, hd' :: tl when hd + hd' > n -> aux l tl
    | hd :: _, hd' :: _ when hd + hd' = n && hd <> hd' -> true
    | _, _ :: tl -> aux l tl
  in
  aux l' (List.tl l')

let rec calc acc l =
  match l with
  | [] -> raise Invalid_input
  | hd :: tl when calc_n_in_l hd acc ->
      calc (List.rev (hd :: List.rev (List.tl acc))) tl
  | hd :: _ -> hd

let day09_1 () =
  let inp = inp () in
  let a, b = List.split_on_index 25 inp in
  calc a b

let sum_cons_numb res l =
  let rec aux n acc l = function
    | [] -> raise Invalid_input
    | hd :: tl when n + hd = res ->
        let l = hd :: acc in
        let min, max =
          (List.fold_left min Int.max_int l, List.fold_left max Int.min_int l)
        in
        min + max
    | hd :: tl when hd + n > res -> aux 0 [] (List.tl l) (List.tl l)
    | hd :: tl -> aux (n + hd) (hd :: acc) l tl
  in
  aux 0 [] l l

let day09_2 () =
  let xx = day09_1 () in
  sum_cons_numb xx (inp ())
