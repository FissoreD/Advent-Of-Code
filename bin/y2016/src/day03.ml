(* https://adventofcode.com/2016/day/3 *)

let cnt () =
  Lib.read_file "16" "03" (fun e ->
      String.split_on_char ' ' e
      |> List.filter (( <> ) "")
      |> List.map int_of_string)

module P1 = struct
  let main cnt =
    List.map (List.sort compare) cnt
    |> List.filter (fun e ->
           match e with [ a; b; c ] -> a + b > c | _ -> false)
    |> List.length
end

module P2 = struct
  let main cnt =
    let new_cnt =
      List.init (List.length cnt) (fun pos ->
          let start_pos = 3 * (pos / 3) in
          let modolus = pos mod 3 in
          Lib.List.
            [
              get start_pos cnt |> get modolus;
              get (start_pos + 1) cnt |> get modolus;
              get (start_pos + 2) cnt |> get modolus;
            ])
    in
    P1.main new_cnt
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
