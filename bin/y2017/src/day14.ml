(* https://adventofcode.com/2017/day/14 *)

let cnt () = Lib.read_file "17" "14" Fun.id |> List.hd

module P1 = struct
  let char_to_hex c = int_of_string ("0x" ^ Lib.string_of_char c)

  let char_to_bin c =
    Lib.(string_start_padding 4 '0' (int_to_bin (char_to_hex c)))

  let hash_to_bin hex_str : string =
    let len = String.length hex_str in
    let rec aux acc = function
      | n when n = len -> acc
      | n -> aux (acc ^ char_to_bin hex_str.[n]) (n + 1)
    in
    aux "" 0

  let compute_matrix stop cnt =
    let arr = Array.make stop "" in
    let rec aux = function
      | n when n = stop -> ()
      | n ->
          let hash = Day10.P2.main (Printf.sprintf "%s-%d" cnt n) in
          let ones = hash_to_bin hash in
          arr.(n) <- ones;
          aux (n + 1)
    in
    aux 0;
    arr

  let main cnt =
    compute_matrix 128 cnt
    |> Array.map ((Fun.flip Lib.count_substring) "1")
    |> Array.fold_left ( + ) 0
end

module P2 = struct
  include P1
  open Hashtbl

  let main cnt =
    let matrix = compute_matrix 128 cnt in
    let pos_to_index = create 2048 in
    let w, h = (String.length matrix.(0), Array.length matrix) in
    let is_one ({ x; y } : Pos.t) = matrix.(y).[x] = '1' in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let pos : Pos.t = { x; y } in
        if is_one pos then add pos_to_index pos (length pos_to_index)
      done
    done;
    let uf = Union_find.create (length pos_to_index) in
    iter
      (fun k v ->
        Pos.neighbors k
        |> List.filter (Pos.is_valid (w, h))
        |> List.filter is_one
        |> List.map (find pos_to_index)
        |> List.iter (Union_find.union uf v))
      pos_to_index;
    uf
    |> Array.mapi (fun pos i -> (pos, i))
    |> Array.fold_left (fun acc (a, b) -> acc + Lib.bool_to_int (a = b)) 0
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
