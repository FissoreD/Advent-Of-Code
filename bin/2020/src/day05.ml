open Lib

let first s = String.sub s 0 7 |> string_2_char_list

let last s = String.sub s 7 3 |> string_2_char_list

let inp () = read_file "day05" (fun l -> (first l, last l))

let rec calc (low, up) l =
  match l with
  | [] -> low
  | 'F' :: tl | 'L' :: tl -> calc (low, low + ((up - low) / 2)) tl
  | _ :: tl -> calc (low + ((up - low) / 2) + 1, up) tl

let calc_seat (a, b) = (calc (0, 127) a * 8) + calc (0, 7) b

let day05_1 () = inp () |> List.map calc_seat |> List.fold_left max 0

let rec find_empty_place = function
  | [] -> raise Invalid_input
  | hd :: hd' :: tl when hd + 1 <> hd' -> hd + 1
  | _ :: tl -> find_empty_place tl

let day05_2 () =
  inp () |> List.map calc_seat |> List.sort compare |> find_empty_place
