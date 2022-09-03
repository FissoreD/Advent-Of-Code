(* https://adventofcode.com/2017/day/6 *)

let cnt () =
  Lib.read_file "17" "06" (fun e ->
      Lib.split_on_space e |> List.map int_of_string)
  |> List.hd

module P1 = struct
  let find_max_pos =
    List.fold_left (fun acc e -> if snd e > snd acc then e else acc) (0, 0)

  let distribute list len =
    let cnt_pos, cnt = find_max_pos list in
    let cnt_to_give = cnt / len in
    let reminder = cnt mod len in
    let add_one n = (len + n - cnt_pos - 1) mod len < reminder in
    let value pos e =
      cnt_to_give
      + Lib.bool_to_int (add_one pos)
      + if pos <> cnt_pos then e else 0
    in
    List.map (fun (pos, e) -> (pos, value pos e)) list

  let main cnt =
    let len = List.length cnt in
    let l = List.mapi (fun pos e -> (pos, e)) cnt in
    let explored = Hashtbl.create 2048 in
    let rec aux acc l =
      if Hashtbl.mem explored l then acc
      else
        let next = distribute l len in
        Hashtbl.add explored l 0;
        aux (acc + 1) next
    in
    aux 0 l
end

module P2 = struct
  let main cnt =
    let len = List.length cnt in
    let l = List.mapi (fun pos e -> (pos, e)) cnt in
    let explored = Hashtbl.create 2048 in
    let rec aux seen acc l =
      if Some l = seen then acc + 1
      else
        let next = P1.distribute l len in
        let already_seen = Hashtbl.mem explored l in
        Hashtbl.add explored l 0;
        aux
          (if seen = None && already_seen then Some l else seen)
          (acc + Lib.bool_to_int (seen <> None))
          next
    in
    aux None 0 l
end

let part1 () = P1.main (cnt ()) |> Lib.print_int
let part2 () = P2.main (cnt ()) |> Lib.print_int
