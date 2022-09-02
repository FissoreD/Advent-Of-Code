(* https://adventofcode.com/2016/day/22 *)
type row = { pos : Pos.t; size : int; used : int; avail : int; use_perc : int }

type grid_info = {
  h : int;
  w : int;
  arr : string option array;
  mutable current_pos : Pos.t;
  mutable content_pos : Pos.t;
}

let cnt () =
  let open List in
  let open Re.Str in
  let i, m = (int_of_string, matched_group) in
  let parse_input l =
    let regex =
      {|/dev/grid/node-x\([0-9]+\)-y\([0-9]+\) +\([0-9]+\)T +\([0-9]+\)T +\([0-9]+\)T +\([0-9]+\)%|}
    in
    search_forward (regexp regex) l 0 |> ignore;
    {
      pos = { x = i (m 1 l); y = i (m 2 l) };
      size = i (m 3 l);
      used = i (m 4 l);
      avail = i (m 5 l);
      use_perc = i (m 6 l);
    }
  in
  Lib.read_file "16" "22" Lib.id |> tl |> tl |> map parse_input

module P1 = struct
  let is_empty { used; _ } = used = 0
  let fit current hd = current.used <= hd.avail

  let main cnt =
    let sorted_by_used_decr =
      List.sort (fun a b -> compare b.used a.used) cnt
    in
    let sorted_by_avail_decr =
      List.sort (fun a b -> compare b.avail a.avail) cnt
    in
    let rec find_pos current acc = function
      | hd :: tl when fit current hd -> find_pos current (acc + 1) tl
      | l -> (acc, l)
    in
    let rec aux acc pred avail_l = function
      | [] -> acc
      | hd :: tl when is_empty hd -> aux acc pred avail_l tl
      | hd :: tl ->
          let acc1, avail = find_pos hd 0 avail_l in
          aux
            (acc + (acc1 + pred) - Lib.bool_to_int (fit hd hd))
            (pred + acc1) avail tl
    in
    aux 0 0 sorted_by_avail_decr sorted_by_used_decr
end

module P2 = struct
  let get_value w arr ({ x; y } : Pos.t) = arr.((w * y) + x)
  let int_to_pos i w : Pos.t = { x = i mod w; y = i / w }

  let build_matrix cnt =
    let arr =
      List.sort (fun a b -> compare (a.pos.y, a.pos.x) (b.pos.y, b.pos.x)) cnt
      |> Array.of_list
    in
    let incr cond v = if cond then v + 1 else v in
    let h, w, start_pos =
      Array.fold_left
        (fun (h, w, s) { pos; used; _ } ->
          (incr (pos.x = 0) h, incr (pos.y = 0) w, if used = 0 then pos else s))
        (0, 0, { x = 0; y = 0 })
        arr
    in
    let arr =
      Array.init (w * h) (fun i ->
          let pos = int_to_pos i w in
          let content = (get_value w arr pos).used in
          if content <= (get_value w arr start_pos).size then Some "." else None)
    in
    { w; h; current_pos = start_pos; arr; content_pos = { x = w - 1; y = 0 } }

  let print { w; arr; current_pos; content_pos; _ } =
    Array.iteri
      (fun pos e ->
        (match e with
        | None -> "#"
        | Some e ->
            if pos mod w = 0 then print_newline ();
            if int_to_pos pos w = current_pos then "_"
            else if int_to_pos pos w = content_pos then "G"
            else e)
        |> print_string)
      arr

  let shortest_path { w; arr; current_pos; content_pos; _ } =
    Pos.shortest_path_len (Lib.Array.to_matrix w arr) current_pos content_pos

  let main cnt =
    let maze = build_matrix cnt in
    let dist = shortest_path maze in
    ((maze.w - 2) * 5) + dist
end

let part1 () = P1.main (cnt ()) |> Printf.printf "%d\n"
let part2 () = P2.main (cnt ()) |> Printf.printf "%d\n"
