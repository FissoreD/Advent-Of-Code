(* https://adventofcode.com/2016/day/19 *)

let cnt () = Lib.read_file "16" "19" int_of_string |> List.hd

module P1 = struct
  let main cnt =
    let min = 1 lsl Lib.two_power_floor cnt in
    ((cnt - min) * 2) + 1
end

module P2 = struct
  let calc_max cnt = 3. ** Float.ceil (log cnt /. log 3.) |> int_of_float

  let main cnt =
    let max = calc_max @@ float cnt in
    if cnt <= Lib.double max / 3 then cnt - (max / 3) else Lib.double cnt - max
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
