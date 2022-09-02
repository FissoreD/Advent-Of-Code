(* https://adventofcode.com/2015/day/4 *)

let cnt () =
  match Lib.read_file "15" "04" Lib.id with
  | [ a ] -> a
  | _ -> raise Lib.Invalid_input

let find_brute_force s len =
  let prefix = String.init len (fun _ -> '0') in
  let rec aux n =
    let dig = s ^ (n |> string_of_int) |> Lib.md5_to_hex in
    if String.starts_with ~prefix dig then n else aux (n + 1)
  in
  aux 0

let part1 () = find_brute_force (cnt ()) 5 |> print_int
let part2 () = find_brute_force (cnt ()) 6 |> print_int