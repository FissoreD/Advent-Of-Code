open Lib
module SET06 = Set.Make (Char)

let inp () = read_file "day06" (fun a -> a) |> remove_empty_line

let calc =
  List.map (fun elt ->
      Str.global_replace (Str.regexp " ") "" elt
      |> string_2_char_list |> SET06.of_list)

let day06_1 () =
  inp () |> calc |> List.fold_left (fun acc elt -> acc + SET06.cardinal elt) 0

let calc' l =
  let set =
    List.map
      (fun elt ->
        String.split_on_char ' ' elt
        |> List.map string_2_char_list
        |> List.map SET06.of_list)
      l
  in
  let full_set = List.init 27 (fun i -> Char.chr (Char.code 'a' + i)) in
  List.map
    (fun elt -> List.fold_left SET06.inter (SET06.of_list full_set) elt)
    set

let day06_2 () =
  inp () |> calc' |> List.fold_left (fun acc elt -> SET06.cardinal elt + acc) 0
