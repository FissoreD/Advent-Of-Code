(* https://adventofcode.com/2015/day/25 *)

let rowG, columnG =
  match Lib.read_file "15" "25" String.trim |> List.map int_of_string with
  | h1 :: h2 :: _ -> (h1, h2)
  | _ -> invalid_arg "Error"

let next x = x * 252533 mod 33554393

module P1 = struct
  let next_pos (r, c) = if r = 1 then (c + 1, 1) else (r - 1, c + 1)

  let main start =
    let rec aux ((r, c) as pos) value =
      if r = rowG && c = columnG then value else aux (next_pos pos) (next value)
    in
    aux (1, 1) start
end

let part1 () = P1.main 20151125 |> string_of_int
let part2 () = "Nope"
