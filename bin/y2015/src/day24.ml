(* https://adventofcode.com/2015/day/24 *)

let cnt () = Lib.read_file "15" "24" int_of_string

module P1 = struct
  let main parts cnt =
    let obj = List.fold_left ( + ) 0 cnt / parts in
    let res, groups = (ref max_int, ref max_int) in
    let rec aux acc mul gr l =
      if acc = obj then (
        if gr < !groups then (
          res := mul;
          groups := gr)
        else if gr = !groups then res := min mul !res)
      else if acc < obj && gr < !groups then
        match l with
        | [] -> ()
        | hd :: tl ->
            aux (acc + hd) (mul * hd) (gr + 1) tl;
            aux acc mul gr tl
    in
    aux 0 1 0 cnt;
    !res
end

let part1 () = P1.main 3 (cnt ()) |> string_of_int
let part2 () = P1.main 4 (cnt ()) |> string_of_int
