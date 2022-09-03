(* https://adventofcode.com/2017/day/5 *)

let cnt () = Lib.read_file "17" "05" int_of_string |> Array.of_list

module P1 = struct
  let main cnt =
    let pos, len, step = (ref 0, Array.length cnt, ref 0) in
    while !pos < len do
      cnt.(!pos) <- cnt.(!pos) + 1;
      pos := !pos + cnt.(!pos) - 1;
      incr step
    done;
    !step
end

module P2 = struct
  let main cnt =
    let pos, len, step = (ref 0, Array.length cnt, ref 0) in
    while !pos < len do
      let cnt' = cnt.(!pos) in
      cnt.(!pos) <- (cnt' + if cnt' >= 3 then -1 else 1);
      pos := !pos + cnt';
      incr step
    done;
    !step
end

let part1 () = P1.main (cnt ()) |> Lib.print_int
let part2 () = P2.main (cnt ()) |> Lib.print_int
