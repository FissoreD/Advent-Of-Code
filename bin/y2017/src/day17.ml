(* https://adventofcode.com/2017/day/17 *)

let cnt () = Lib.read_file "17" "17" Fun.id |> List.hd |> int_of_string

module P1 = struct
  let rec find_nth_cell tbl cnt_pos = function
    | 0 -> cnt_pos
    | n -> find_nth_cell tbl (Hashtbl.find tbl cnt_pos) (n - 1)

  let main step =
    let tbl = Hashtbl.create 2018 in
    Hashtbl.add tbl 0 0;
    let rec aux = function
      | 2018 -> Hashtbl.find tbl 2017
      | n ->
          let nth_cell = find_nth_cell tbl (n - 1) step in
          Hashtbl.replace tbl n (Hashtbl.find tbl nth_cell);
          Hashtbl.replace tbl nth_cell n;
          aux (n + 1)
    in
    aux 1
end

module P2 = struct
  let main step =
    let rec aux old pos = function
      | 50_000_001 -> old
      | n ->
          let pos = (pos + step) mod n in
          aux (if pos = 0 then n else old) (pos + 1) (n + 1)
    in
    aux 1 0 1
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
