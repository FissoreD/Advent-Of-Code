(* https://adventofcode.com/2017/day/15 *)

let cnt () =
  match
    Lib.read_file "17" "15" (fun e -> Lib.find_all_ints_in_string e |> List.hd)
  with
  | a :: [ b ] -> (a, b)
  | _ -> failwith "Should not be here"

module P1 = struct
  let next_step (a, b) = (a * 16807 mod 2147483647, b * 48271 mod 2147483647)
  let same (a, b) = a land 65535 = b land 65535

  let main ?(next_step = next_step) ?(max = 40_000_000) =
    let rec main acc pos couple =
      if pos = max then acc
      else
        let next = next_step couple in
        main (acc + Lib.bool_to_int (same next)) (pos + 1) next
    in
    main 0 0
end

module P2 = struct
  let rec next_step x factor bits =
    let next = x * factor mod 2147483647 in
    if next land bits = 0 then next else next_step next factor bits

  let next_step (a, b) = (next_step a 16807 3, next_step b 48271 7)
  let main = P1.main ~next_step ~max:5_000_000
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
