(* https://adventofcode.com/2018/day/8 *)

let cnt () =
  Lib.read_file "18" "08" Lib.find_all_ints_in_string
  |> List.hd |> Array.of_list

module P1 = struct
  let sum_meta arr start len =
    let rec aux acc = function
      | i when i = len -> acc
      | i -> aux (acc + arr.(start + i)) (i + 1)
    in
    aux 0 0

  let rec parse_node (start, sum) arr =
    let node_nb, meta_len = (arr.(start), arr.(start + 1)) in
    let build_res st sum = (st + meta_len, sum + sum_meta arr st meta_len) in
    if node_nb = 0 then build_res (start + 2) sum
    else
      let rec aux (start, sum) = function
        | n when n = node_nb -> build_res start sum
        | n -> aux (parse_node (start, sum) arr) (n + 1)
      in
      aux (start + 2, sum) 0

  let main cnt = parse_node (0, 0) cnt |> snd
end

module P2 = struct
  let add_ind (pos, children) arr len =
    let rec aux acc = function
      | n when n = len -> acc
      | n -> (
          match children.(arr.(pos + n) - 1) with
          | exception _ -> aux acc (n + 1)
          | x -> aux (acc + x) (n + 1))
    in
    aux 0 0

  let rec parse_node st arr =
    let node_nb, meta_len = (arr.(st), arr.(st + 1)) in
    if node_nb = 0 then (st + 2 + meta_len, P1.sum_meta arr (st + 2) meta_len)
    else
      let children = Array.make node_nb 0 in
      let rec aux pos = function
        | n when n = node_nb -> pos
        | n ->
            let pos, sum = parse_node pos arr in
            children.(n) <- sum;
            aux pos (n + 1)
      in
      let pos = aux (st + 2) 0 in
      (pos + meta_len, add_ind (pos, children) arr meta_len)

  let main cnt = parse_node 0 cnt |> snd
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
