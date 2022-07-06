open Lib

let inp () = [ read_file "day17" string_2_char_list ]

let get3D' l (x, y, z) = List.(get z l |> get y |> get x)

let get3D l elt = try get3D' l elt with _ -> '.'

let get4D l (x, y, z, w) = try get3D' (List.get w l) (x, y, z) with _ -> '.'

let neigh pos =
  List.init 26 (fun i ->
      let i = if i >= 13 then i + 1 else i in
      ((i / 9 mod 3) - 1, (i / 3 mod 3) - 1, (i mod 3) - 1))
  |> List.map (( +++ ) pos)

let calc_one_step l pos =
  neigh pos |> List.map (fun elt -> get3D l elt) |> List.count '#'

let rec calc n l =
  if n = 0 then l
  else
    calc (n - 1)
      (let open List in
      let x, y, z = (hd (hd l) |> length, hd l |> length, l |> length) in
      List.init (z + 2) (fun z ->
          List.init (y + 2) (fun y ->
              List.init (x + 2) (fun x ->
                  let pos = (x - 1, y - 1, z - 1) in
                  let actives = calc_one_step l pos in
                  match get3D l pos with
                  | '.' when actives = 3 -> '#'
                  | '#' when actives <> 2 && actives <> 3 -> '.'
                  | n -> n))))

let day17_1 () = List.(inp () |> calc 6 |> flatten |> flatten |> count '#')

let neigh' pos =
  List.init 80 (fun i ->
      let i = if i >= 40 then i + 1 else i in
      ((i / 27 mod 3) - 1, (i / 9 mod 3) - 1, (i / 3 mod 3) - 1, (i mod 3) - 1))
  |> List.map (( ++++ ) pos)

let calc_one_step' l pos =
  neigh' pos |> List.map (fun elt -> get4D l elt) |> List.count '#'

let rec calc' n l =
  if n = 0 then l
  else
    calc' (n - 1)
      (let open List in
      let x, y, z, w =
        ( hd (hd (hd l)) |> length,
          hd (hd l) |> length,
          hd l |> length,
          l |> length )
      in
      List.init (w + 2) (fun w ->
          List.init (z + 2) (fun z ->
              List.init (y + 2) (fun y ->
                  List.init (x + 2) (fun x ->
                      let pos = (x - 1, y - 1, z - 1, w - 1) in
                      let actives = calc_one_step' l pos in
                      match get4D l pos with
                      | '.' when actives = 3 -> '#'
                      | '#' when actives <> 2 && actives <> 3 -> '.'
                      | n -> n)))))

let day17_2 () =
  List.([ inp () ] |> calc' 6 |> flatten |> flatten |> flatten |> count '#')

let _ = day17_2 () |> print_int
