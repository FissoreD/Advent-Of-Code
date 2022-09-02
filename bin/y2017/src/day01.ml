(* https://adventofcode.com/2017/day/1 *)

let cnt = Lib.read_file "17" "01" (fun e -> Lib.string_2_char_list e) |> List.hd

module P1 = struct
  let int_of_char c = int_of_char c - int_of_char '0'

  let main () =
    let rec aux acc = function
      | [] -> acc
      | [ a ] when a = List.hd cnt -> acc + int_of_char a
      | a :: b :: tl when a = b -> aux (acc + int_of_char a) (b :: tl)
      | _ :: tl -> aux acc tl
    in
    aux 0 cnt
end

module P2 = struct
  let main () =
    let cnt = Array.of_list cnt in
    let counter = ref 0 in
    let len = Array.length cnt in
    for i = 0 to len - 1 do
      if cnt.(i) = cnt.((i + Lib.halve len) mod len) then
        counter := !counter + P1.int_of_char cnt.(i)
    done;
    !counter
end

let part1 () = P1.main () |> Lib.print_int
let part2 () = P2.main () |> Lib.print_int
