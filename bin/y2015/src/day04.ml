(* https://adventofcode.com/2015/day/4 *)

let cnt () = Lib.read_file "15" "04" Fun.id |> List.hd

let find_brute_force s len =
  let prefix = String.init len (Fun.const '0') in
  let rec aux n =
    let hash = s ^ (n |> string_of_int) |> Lib.md5_to_hex in
    if String.starts_with ~prefix hash then n else aux (n + 1)
  in
  aux 0

let part1 () = find_brute_force (cnt ()) 5 |> string_of_int
let part2 () = find_brute_force (cnt ()) 6 |> string_of_int