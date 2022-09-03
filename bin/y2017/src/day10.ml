(* https://adventofcode.com/2017/day/10 *)

let cnt () = Lib.read_file "17" "10" Lib.id |> List.hd

module P1 = struct
  let reverse_sub_arr arr len start =
    let arr_len = Array.length arr in
    for i = 0 to (len / 2) - 1 do
      let p1 = (start + i) mod arr_len in
      let p2 = (len - 1 + start - i) mod arr_len in
      let tmp = arr.(p1) in
      arr.(p1) <- arr.(p2);
      arr.(p2) <- tmp
    done

  let main ?(len = 256) (cnt : string) =
    let arr = Array.init len Lib.id in
    let len = Array.length arr in
    let rec aux pos skip = function
      | [] -> arr.(0) * arr.(1)
      | hd :: tl ->
          let new_pos = (pos + skip + hd) mod len in
          reverse_sub_arr arr hd pos;
          aux new_pos (skip + 1) tl
    in
    aux 0 0 (String.split_on_char ',' cnt |> List.map int_of_string)
end

module P2 = struct
  let to_ascii l =
    (Lib.string_2_char_list l |> List.map int_of_char) @ [ 17; 31; 73; 47; 23 ]

  let main ?(len = 256) (cnt : string) =
    let arr = Array.init len Lib.id in
    let len = Array.length arr in
    let cnt' = to_ascii cnt in
    let rec aux (pos, skip) = function
      | [] -> (pos, skip)
      | hd :: tl ->
          let new_pos = (pos + skip + hd) mod len in
          P1.reverse_sub_arr arr hd pos;
          aux (new_pos, skip + 1) tl
    in
    let rec repeat (pos, skip) = function
      | 0 -> ()
      | n -> repeat (aux (pos, skip) cnt') (n - 1)
    in
    repeat (0, 0) 64;
    List.init 16 (fun i ->
        Array.sub arr (16 * i) 16
        |> Array.fold_left (fun acc e -> e lxor acc) 0
        |> Printf.sprintf "%02x")
    |> String.concat ""
end

let part1 () = P1.main (cnt ()) |> Lib.print_int
let part2 () = P2.main (cnt ()) |> print_endline
