(* https://adventofcode.com/2016/day/5 *)

let cnt = Lib.read_file "16" "05" Lib.id |> List.hd

module P1 = struct
  let hash s = Digest.string s |> Digest.to_hex
  let is_valid s = String.sub s 0 5 |> String.for_all (( = ) '0')

  let rec main ?(nmb = 0) ?(res = "") ?(max_len = 8) () =
    if String.length res = max_len then res
    else
      let word = hash (cnt ^ string_of_int nmb) in
      main ~nmb:(nmb + 1)
        ~res:(res ^ if is_valid word then String.sub word 5 1 else "")
        ()
end

module P2 = struct
  let len = 8

  let rec main ?(nmb = 0) ?(res = Array.make len " ") ?(found_len = ref 0) () =
    (* Array.fold_left ( ^ ) "" res |> print_endline; *)
    if !found_len = len then Array.fold_left ( ^ ) "" res
    else
      let word = P1.hash (cnt ^ string_of_int nmb) in
      let pos = String.get word 5 in
      (if P1.is_valid word && String.contains "01234567" pos then
       let pos = String.make 1 pos |> int_of_string in
       if res.(pos) = " " then (
         res.(pos) <- String.sub word 6 1;
         incr found_len));
      main ~nmb:(nmb + 1) ~res ~found_len ()
end

let part1 () = P1.main () |> print_endline
let part2 () = P2.main () |> print_endline
