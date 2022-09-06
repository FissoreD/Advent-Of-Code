(* https://adventofcode.com/2017/day/4 *)

let cnt () = Lib.read_file "17" "04" (String.split_on_char ' ')

module P1 = struct
  module S = Set.Make (String)

  let main cnt =
    List.map (fun e -> S.cardinal (S.of_list e) = List.length e) cnt
    |> List.map Lib.bool_to_int |> List.fold_left ( + ) 0
end

module P2 = struct
  module S = Set.Make (Char)
  module S1 = Set.Make (S)

  let main (cnt : string list list) =
    List.map
      (fun (e : string list) ->
        List.map (fun e -> S.of_list @@ Lib.string_2_char_list e) e
        |> S1.of_list |> S1.cardinal = List.length e)
      cnt
    |> List.map Lib.bool_to_int |> List.fold_left ( + ) 0
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
