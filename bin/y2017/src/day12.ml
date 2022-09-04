(* https://adventofcode.com/2017/day/12 *)

let cnt () =
  let open Re.Str in
  let parse_line l =
    let l = Lib.remove_comma l in
    let l = split (regexp " <-> ") l in
    (List.hd l, List.(hd (tl l)) |> String.split_on_char ' ')
  in
  Lib.read_file "17" "12" parse_line

module P1 = struct
  open Hashtbl

  let rec find_group tbl prog_nb =
    let find_group = find_group tbl in
    let neighs =
      find_opt tbl prog_nb |> Option.to_list |> List.flatten
      |> List.filter (mem tbl)
    in
    remove tbl prog_nb;
    List.(length neighs + (map find_group neighs |> fold_left ( + ) 0))

  let main cnt =
    let tbl = create (List.length cnt) in
    List.iter (fun (a, b) -> add tbl a b) cnt;
    find_group tbl "0"
end

module P2 = struct
  open Hashtbl

  let rec find_group rel prog_nb =
    let find_group = find_group rel in
    let neighs =
      find_opt rel prog_nb |> Option.to_list |> List.flatten
      |> List.filter (mem rel)
    in
    remove rel prog_nb;
    List.iter find_group neighs

  let main cnt =
    let tbl = create (List.length cnt) in
    List.iter (fun (a, b) -> add tbl a b) cnt;
    let rec aux = function
      | [] -> 0
      | (hd, _) :: tl ->
          let add_one = Lib.bool_to_int @@ mem tbl hd in
          find_group tbl hd;
          aux tl + add_one
    in
    aux cnt
end

let part1 () = P1.main (cnt ()) |> Lib.print_int
let part2 () = P2.main (cnt ()) |> Lib.print_int
