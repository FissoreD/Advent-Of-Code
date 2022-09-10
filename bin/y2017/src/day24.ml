(* https://adventofcode.com/2017/day/24 *)

let cnt () =
  Lib.read_file "17" "24" (fun l ->
      match Lib.find_all_ints_in_string l with
      | [ a; b ] -> (a, b)
      | _ -> failwith "Y17-D24 invalid input")
  |> Array.of_list

module P1 = struct
  module Set' = Set.Make (struct
    type t = int * int

    let compare (a1, a2) (b1, b2) =
      compare (max a1 a2, min a1 a2) (max b1 b2, min b1 b2)
  end)

  let find tbl k = Option.value ~default:Set'.empty (Hashtbl.find_opt tbl k)

  let to_tbl cnt =
    let tbl = Hashtbl.create (2 lsr 10) in
    for i = 0 to Array.length cnt - 1 do
      let a, b = cnt.(i) in
      Hashtbl.replace tbl a (Set'.add (a, b) @@ find tbl a);
      if a <> b then Hashtbl.replace tbl b (Set'.add (b, a) @@ find tbl b)
    done;
    tbl

  let main cnt =
    let tbl = to_tbl cnt in
    let rec aux start used acc =
      let usable = Set'.diff (find tbl start) used in
      if Set'.is_empty usable then acc + start
      else if Set'.mem (start, start) usable then
        aux start (Set'.add (start, start) used) (acc + (start * 2))
      else
        Set'.fold
          (fun ((_, next) as c) acc' ->
            max acc' @@ aux next (Set'.add c used) (acc + (start * 2)))
          usable 0
    in
    aux 0 Set'.empty 0
end

module P2 = struct
  include P1

  let main cnt =
    let tbl = to_tbl cnt in
    let max_len = ref 0 in
    let res_list = ref [] in
    let rec aux start used acc len =
      let usable = Set'.diff (find tbl start) used in
      if Set'.is_empty usable then (
        if len = !max_len then res_list := (acc + start) :: !res_list
        else if len > !max_len then (
          max_len := len;
          res_list := [ acc + start ]))
      else if Set'.mem (start, start) usable then
        aux start (Set'.add (start, start) used) (acc + (start * 2)) (len + 1)
      else
        Set'.iter
          (fun ((_, next) as c) ->
            aux next (Set'.add c used) (acc + (start * 2)) (len + 1))
          usable
    in
    aux 0 Set'.empty 0 0;
    List.fold_left max 0 !res_list
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
