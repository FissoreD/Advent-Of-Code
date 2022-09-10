(* https://adventofcode.com/2018/day/1 *)

let cnt () = Lib.read_file "18" "01" int_of_string

module P1 = struct
  let main = List.fold_left ( + ) 0
end

module P2 = struct
  let main cnt =
    let tbl = Hashtbl.create 2048 in
    let rec aux acc = function
      | _ when Hashtbl.mem tbl acc -> acc
      | [] -> aux acc cnt
      | hd :: tl ->
          Hashtbl.add tbl acc 0;
          let acc = acc + hd in
          aux acc tl
    in
    aux 0 cnt
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
