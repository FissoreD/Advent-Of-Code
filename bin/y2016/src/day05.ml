(* https://adventofcode.com/2016/day/5 *)

let cnt () = Lib.read_file "16" "05" Fun.id |> List.hd

module P1 = struct
  let is_valid s = String.sub s 0 5 |> String.for_all (( = ) '0')

  let rec main ?(nmb = 0) ?(res = "") ?(max_len = 8) cnt =
    if String.length res = max_len then res
    else
      let word = Lib.md5_to_hex (cnt ^ string_of_int nmb) in
      main ~nmb:(nmb + 1)
        ~res:(res ^ if is_valid word then String.sub word 5 1 else "")
        cnt
end

module P2 = struct
  let len = 8

  let rec main ?(nmb = 0) ?(res = Array.make len " ") ?(found_len = ref 0) cnt =
    if !found_len = len then Array.fold_left ( ^ ) "" res
    else
      let word = Lib.md5_to_hex (cnt ^ string_of_int nmb) in
      let pos = String.get word 5 in
      (if P1.is_valid word && String.contains "01234567" pos then
       let pos = String.make 1 pos |> int_of_string in
       if res.(pos) = " " then (
         res.(pos) <- String.sub word 6 1;
         incr found_len));
      main ~nmb:(nmb + 1) ~res ~found_len cnt
end

let part1 () = P1.main (cnt ())
let part2 () = P2.main (cnt ())
