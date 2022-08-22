(* https://adventofcode.com/2016/day/6 *)

open Lib

let cnt =
  Lib.read_file "16" "06" (fun e -> string_2_char_list e |> Array.of_list)
  |> Array.of_list

module P1 = struct
  let main ?(sort_fun = compare) () =
    let res = ref "" in
    for i = 0 to Array.length cnt.(0) - 1 do
      let letters =
        Array.init 26 (fun i -> (letter_pos_to_char i |> string_of_char, ref 0))
      in
      for j = 0 to Array.length cnt - 1 do
        let pos = char_to_letter_pos cnt.(j).(i) in
        incr (snd letters.(pos))
      done;
      Array.sort (fun a b -> sort_fun !(snd b) !(snd a)) letters;
      res := !res ^ fst letters.(0)
    done;
    !res
end

module P2 = struct
  let main =
    P1.main ~sort_fun:(fun a b ->
        if a = 0 then min_int else if b = 0 then max_int else b - a)
end

let part1 () = P1.main () |> print_endline
let part2 () = P2.main () |> print_endline
