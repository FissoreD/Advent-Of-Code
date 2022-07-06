open Lib
module Map15 = Map.Make (Int)

let inp () =
  read_file "day15" (fun elt ->
      String.split_on_char ',' elt |> List.map int_of_string)
  |> List.hd

let calc_next (map, elt, pos) =
  try
    let new_elt = pos - Map15.find elt map in
    (Map15.add elt pos map, new_elt, pos + 1)
  with _ -> (Map15.add elt pos map, 0, pos + 1)

let calc l =
  let last, l =
    let rev = List.rev l in
    (List.hd rev, List.rev (List.tl rev))
  in
  ( List.mapi (fun pos elt -> (elt, pos + 1)) l
    |> List.fold_left (fun acc (elt, pos) -> Map15.add elt pos acc) Map15.empty,
    last,
    List.length l + 1 )

let calc_rec acc ((m, _, l) as i) =
  let rec aux acc i =
    match acc with 0 -> i | n -> aux (n - 1) (calc_next i)
  in
  if acc - l < 0 then
    Map15.fold (fun elt ind acc -> if ind = acc then elt else acc) m 0
  else match aux (acc - l) i with _, e, _ -> e

let day15_1 () = inp () |> calc |> calc_rec 2020

let day15_2 () = inp () |> calc |> calc_rec 30000000
