open Lib

let inp () = read_file "day03" string_2_char_list

let get mat (a, b) = mat |> List.get b |> List.get a = '#' |> bool_to_int

let rec calc pos incr l =
  match pos with
  | _, b when b >= List.length l -> 0
  | (a, b) as pos ->
      let newA = a mod List.length (List.hd l) in
      get l (newA, b) + calc (pos ++ incr) incr l

let day03_1 () = inp () |> calc (0, 0) (3, 1)

let day03_2 () =
  let mult_inp = [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ] in
  let mat = inp () in
  let f x = calc (0, 0) x mat in
  List.map f mult_inp |> List.fold_left ( * ) 1
