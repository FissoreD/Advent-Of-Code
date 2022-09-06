(* https://adventofcode.com/2016/day/16 *)

let cnt () =
  Lib.read_file "16" "16" (fun l ->
      Lib.string_2_char_list l |> List.map (( = ) '1'))
  |> List.hd |> Array.of_list

module P1 = struct
  open Lib

  let height =
    let rec aux power = function
      | x when is_even x && x > 1 -> aux (double power) (halve x)
      | x -> (power, x)
    in
    aux 1

  let get_bit div reminder inp inp_rev len : bool =
    if reminder = len then ((div land ~-div) lsl 1) land div <> 0
    else (if is_odd div then inp else inp_rev).(reminder)

  let next (div, reminder) m =
    if reminder + 1 = m then (div + 1, 0) else (div, reminder + 1)

  let main ?(len = 272) inp =
    let arr_len = Array.length inp in
    let cnt_rev = Array.init arr_len (fun i -> not inp.(arr_len - i - 1)) in
    let checksum_str_len, res_len = height len in
    let res = Array.make res_len false in
    let op = ref (1, 0) in
    for i = 0 to res_len - 1 do
      let bit = ref true in
      for _ = 0 to checksum_str_len - 1 do
        bit := !bit <> get_bit (fst !op) (snd !op) inp cnt_rev arr_len;
        op := next !op (arr_len + 1)
      done;
      res.(i) <- !bit
    done;
    res |> Array.fold_left (fun acc e -> acc ^ if e then "1" else "0") ""
end

let part1 () = P1.main (cnt ())
let part2 () = P1.main ~len:35651584 (cnt ())
