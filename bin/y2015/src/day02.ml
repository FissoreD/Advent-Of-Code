(* https://adventofcode.com/2015/day/2 *)

let cnt =
  Lib.read_file "15" "02" Lib.id
  |> List.map (String.split_on_char 'x')
  |> List.map (function
       | [] -> raise Lib.Invalid_input
       | [ l; w; h ] ->
           (l |> int_of_string, w |> int_of_string, h |> int_of_string)
       | f :: _ ->
           print_endline ("Here" ^ f);
           raise Lib.Invalid_input)

module Part1Module = struct
  let total_area_counter cnt =
    List.map
      (fun (l, w, h) ->
        let lw, wh, hl = (l * w, w * h, h * l) in
        (2 * (lw + wh + hl)) + min (min lw wh) hl)
      cnt
    |> List.fold_left ( + ) 0
end

module Part2Module = struct
  let total_area_counter cnt =
    List.map
      (fun (l, w, h) ->
        (l * w * h)
        + 2
          *
          match List.sort compare [ l; w; h ] with
          | a :: b :: _ -> a + b
          | _ -> raise Lib.Invalid_input)
      cnt
    |> List.fold_left ( + ) 0
end

let part1 () = cnt |> Part1Module.total_area_counter |> print_int
let part2 () = cnt |> Part2Module.total_area_counter |> print_int
