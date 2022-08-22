(* https://adventofcode.com/2015/day/10 *)

let cnt =
  Lib.read_file "15" "10" Lib.id
  |> List.hd |> Lib.string_2_char_list
  |> List.map Lib.string_of_char
  |> List.map int_of_string

module P1 = struct
  let look_and_say cnt =
    let rec aux (cmp, acc) : int list -> int list = function
      | [] -> acc |> List.rev
      | h1 :: h2 :: tl when h1 = h2 -> aux (cmp + 1, acc) (h2 :: tl)
      | hd :: tl -> aux (1, hd :: cmp :: acc) tl
    in
    aux (1, []) cnt

  let rec evolve_n_times n inp =
    match n with 0 -> inp | n -> evolve_n_times (n - 1) (look_and_say inp)

  let main cnt = evolve_n_times 40 cnt |> List.length
end

module P2 = struct
  let main cnt = P1.evolve_n_times 50 cnt |> List.length
end

let part1 () = P1.main cnt |> print_int
let part2 () = P2.main cnt |> print_int