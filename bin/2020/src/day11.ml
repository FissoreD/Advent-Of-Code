open Lib

let read_cell l (x, y) = try List.(get y l |> get x) with _ -> '.'

let inp () = read_file "day11" string_2_char_list

let neigh pos l =
  let open List in
  init 8 (fun elt ->
      let elt = elt + if elt >= 4 then 1 else 0 in
      pos ++ ((elt / 3 mod 3) - 1, (elt mod 3) - 1))

let rec calc l =
  let l' =
    List.(
      map
        (fun l' ->
          map
            (fun elt ->
              let x = neigh elt l |> map (read_cell l) |> count '#' in
              match read_cell l elt with
              | 'L' when x = 0 -> '#'
              | '#' when x > 3 -> 'L'
              | z -> z)
            l')
        (init (length l) (fun y -> init (length (hd l)) (fun x -> (x, y)))))
  in
  if l' = l then l else calc l'

let day11_1 () = inp () |> calc |> List.flatten |> List.count '#'

let first_pos l x step =
  let rec aux ((x', y') as pos') =
    try
      let res = List.(get y' l |> get x') in
      if res <> '.' then res else aux (step ++ pos')
    with _ -> '.'
  in
  aux (step ++ x)

let neigh' () =
  let open List in
  init 8 (fun elt ->
      let elt = elt + if elt >= 4 then 1 else 0 in
      ((elt / 3 mod 3) - 1, (elt mod 3) - 1))

let rec calc' l =
  let l' =
    List.(
      map
        (fun l' ->
          map
            (fun elt ->
              let x = neigh' () |> map (first_pos l elt) |> count '#' in
              match read_cell l elt with
              | 'L' when x = 0 -> '#'
              | '#' when x > 4 -> 'L'
              | z -> z)
            l')
        (init (length l) (fun y -> init (length (hd l)) (fun x -> (x, y)))))
  in
  if l' = l then l else calc' l'

let day11_2 () = inp () |> calc' |> List.flatten |> List.count '#'

let _ = day11_2 () |> print_int
