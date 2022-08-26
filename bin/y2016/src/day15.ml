(* https://adventofcode.com/2016/day/15 *)

type disk = { pos : int; size : int }

let cnt : disk list =
  let open Re.Str in
  let i = int_of_string in
  let func l =
    search_forward (regexp {|.*s \([0-9]+\).*n \([0-9]+\)\.|}) l 0 |> ignore;
    { pos = i @@ matched_group 2 l; size = i @@ matched_group 1 l }
  in
  Lib.read_file "16" "15" func

module P1 = struct
  let next_pos { pos; size } index x = (x + index + pos) mod size

  let is_valid dl x incr =
    let fst = next_pos (List.hd dl) 1 x in
    let rec aux index incr = function
      | [] -> (true, incr)
      | hd :: tl ->
          let same_prefix = next_pos hd index x in
          if fst <> same_prefix then (false, incr)
          else
            aux (index + 1)
              (Lib.lcm incr @@ Lib.lcm (List.hd dl).size hd.size)
              tl
    in
    aux 2 incr (List.tl dl)

  let main ?(cnt = cnt) () =
    let rec aux x incr =
      let a, b = is_valid cnt x incr in
      if a then x else aux (x + b) b
    in
    aux 0 1
end

module P2 = struct
  let cnt = cnt @ [ { pos = 0; size = 11 } ]
  let main = P1.main ~cnt
end

let part1 () = P1.main () |> Printf.printf "%d\n"
let part2 () = P2.main () |> Printf.printf "%d\n"
