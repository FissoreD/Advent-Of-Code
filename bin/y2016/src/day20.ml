(* https://adventofcode.com/2016/day/20 *)

let cnt () =
  Lib.read_file "16" "20" (fun e ->
      String.split_on_char '-' e |> List.map int_of_string |> function
      | h1 :: [ h2 ] -> (h1, h2)
      | _ -> invalid_arg "Invalid input")
  |> List.sort compare

module P1 = struct
  let main cnt =
    let rec aux (a, b) = function
      | (c, d) :: tl when c <= b + 1 -> aux (min a c, max b d) tl
      | _ -> b + 1
    in
    aux (0, 0) cnt
end

module P2 = struct
  let main cnt =
    let rec aux acc (a, b) = function
      | [] -> acc
      | (_, d) :: _ when d > 4294967295 -> acc
      | (c, d) :: tl when c <= b + 1 -> aux acc (a, max b d) tl
      | l -> aux (acc + 1) (a, b + 1) l
    in
    aux 0 (0, 0) cnt
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
