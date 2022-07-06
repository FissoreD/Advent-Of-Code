let cnt = Lib.read_file "15" "08" Lib.id

module P1 = struct
  let rec len_char_list acc = function
    | [] -> acc
    | '"' :: tl -> len_char_list acc tl
    | '\\' :: 'x' :: _ :: _ :: tl | '\\' :: _ :: tl | _ :: tl ->
        len_char_list (acc + 1) tl

  let main cnt =
    List.map
      (fun a -> (String.length a, Lib.string_2_char_list a |> len_char_list 0))
      cnt
    |> List.fold_left (fun acc (a, b) -> acc + a - b) 0
end

module P2 = struct
  let rec len_char_list acc = function
    | [] -> acc + 2
    | '"' :: tl | '\\' :: tl -> len_char_list (acc + 2) tl
    | _ :: tl -> len_char_list (acc + 1) tl

  let main cnt =
    List.map
      (fun a -> (Lib.string_2_char_list a |> len_char_list 0, String.length a))
      cnt
    |> List.fold_left (fun acc (a, b) -> acc + a - b) 0
end

let part1 () = P1.main cnt |> print_int
let part2 () = P2.main cnt |> print_int