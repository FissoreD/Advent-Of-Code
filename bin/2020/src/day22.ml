open Lib
module S = Set.Make (Int)

let inp () =
  let i = read_file "day22" id in
  let rec aux acc = function
    | [] -> [ List.rev acc ]
    | "" :: tl -> List.rev acc :: aux [] tl
    | hd :: tl when hd.[0] = 'P' -> aux acc tl
    | hd :: tl -> aux (int_of_string hd :: acc) tl
  in
  aux [] i

let rec my_map2 = function
  | [], l | l, [] -> l
  | hd1 :: tl1, hd2 :: tl2 when hd1 < hd2 ->
      my_map2 (tl1, tl2 @ [ hd2 ] @ [ hd1 ])
  | hd1 :: tl1, hd2 :: tl2 -> my_map2 (tl1 @ [ hd1 ] @ [ hd2 ], tl2)

let day22_1 () =
  let i = inp () in
  let a = (List.hd i, List.hd (List.tl i)) in
  let len = List.length (fst a) + List.length (snd a) in
  my_map2 a
  |> List.mapi (fun ind elt -> (len - ind) * elt)
  |> List.fold_left ( + ) 0

let a = ref 0

let rec my_map2' x mem =
  let x = if List.mem x mem then (fst x, []) else x in
  match x with
  | hd1 :: tl1, hd2 :: tl2 when hd1 <= List.length tl1 && hd2 <= List.length tl2
    ->
      my_map2'
        (let sub1 = List.sub 0 hd1 tl1 in
         let sub2 = List.sub 0 hd2 tl2 in
         match my_map2' (sub1, sub2) [] with
         | [], _ -> (tl1, tl2 @ [ hd2 ] @ [ hd1 ])
         | _ -> (tl1 @ [ hd1 ] @ [ hd2 ], tl2))
        (x :: mem)
  | hd1 :: tl1, hd2 :: tl2 when hd1 < hd2 ->
      my_map2' (tl1, tl2 @ [ hd2 ] @ [ hd1 ]) (x :: mem)
  | hd1 :: tl1, hd2 :: tl2 -> my_map2' (tl1 @ [ hd1 ] @ [ hd2 ], tl2) (x :: mem)
  | x -> x

let day22_2 () =
  let i = inp () in
  let a = (List.hd i, List.hd (List.tl i)) in
  let len = List.length (fst a) + List.length (snd a) in
  (match my_map2' a [] with [], x | x, _ -> x)
  |> List.mapi (fun ind elt -> (len - ind) * elt)
  |> List.fold_left ( + ) 0

let _ = day22_2 () |> print_int
