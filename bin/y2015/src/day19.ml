(* https://adventofcode.com/2015/day/19 *)

module MySet = Set.Make (struct
  type t = char list

  let compare = compare
end)

let cnt () =
  let res = Lib.read_file "15" "19" Fun.id |> List.rev in
  let str = List.hd res |> Lib.string_2_char_list in
  let map =
    List.tl res |> List.tl
    |> List.map Re.Str.(split_delim (regexp " => "))
    |> List.map (function
         | [ h1; h2 ] ->
             (h1 |> Lib.string_2_char_list, h2 |> Lib.string_2_char_list)
         | _ -> invalid_arg "Parsing problem")
  in
  (str, map)

module P1 = struct
  let rec same_prefix : char list * char list -> bool * char list = function
    | [], [] -> (true, [])
    | [ h1 ], h2 :: tl | h1 :: tl, [ h2 ] -> (h1 = h2, tl)
    | h1 :: t1, h2 :: t2 when h1 = h2 -> same_prefix (t1, t2)
    | _ -> (false, [])

  let rec replaceOne (pred : char list) (key, value) : char list -> char list =
    function
    | [] -> []
    | hd :: tl as l ->
        if List.length l < List.length key then pred @ l
        else
          let same, suff = same_prefix (key, l) in
          if same then pred @ value @ suff
          else replaceOne (pred @ [ hd ]) (key, value) tl

  let replace (s : char list) ((a, b) : char list * char list) =
    let rec aux acc (pred : char list) = function
      | [] -> acc
      | hd :: tl as l ->
          if List.length l < List.length a then acc
          else
            let same, suff = same_prefix (a, l) in
            aux
              (if same then (pred @ b @ suff) :: acc else acc)
              (pred @ [ hd ]) tl
    in
    aux [] [] s

  let main ((s : char list), (l : (char list * char list) list)) =
    List.map (replace s) l |> List.flatten |> MySet.of_list |> MySet.cardinal
end

module P2 = struct
  let count_maj str =
    Lib.string_2_char_list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    |> List.map Lib.string_of_char
    |> List.map (Lib.count_substring str)
    |> List.fold_left ( + ) 0

  let count_Rn_Ar str =
    List.map (Lib.count_substring str) [ "Rn"; "Ar" ] |> List.fold_left ( + ) 0

  let count_Y str = Lib.count_substring str "Y"
  let main str = count_maj str - count_Rn_Ar str - (2 * count_Y str) - 1
end

let part1 () = P1.main (cnt ()) |> print_int
let part2 () = P2.main (cnt () |> fst |> Lib.char_list_2_string) |> print_int
