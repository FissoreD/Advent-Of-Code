(* https://adventofcode.com/2018/day/10 *)

let cnt () =
  Lib.read_file "18" "10" (fun line ->
      match Lib.find_all_ints_in_string line with
      | x :: y :: vx :: [ vy ] ->
          (({ x; y }, { x = vx; y = vy }) : Pos.t * Pos.t)
      | _ -> invalid_arg "Y18D10 invalid input")

module P1 = struct
  type dim = { w : int; h : int; min_x : int; min_y : int }

  let calc_size cnt =
    let min_x, min_y, max_x, max_y =
      List.fold_left
        (fun (min_x, min_y, max_x, max_y) ((p1 : Pos.t), _) ->
          (min min_x p1.x, min min_y p1.y, max max_x p1.x, max max_y p1.y))
        (max_int, max_int, 0, 0) cnt
    in
    let w, h = (max_x - min_x + 1, max_y - min_y + 1) in
    { w; h; min_x; min_y }

  let print cnt dim =
    let list =
      List.sort
        (fun ((p1 : Pos.t), _) ((p2 : Pos.t), _) ->
          compare (p1.y, p1.x) (p2.y, p2.x))
        cnt
    in
    let w, h = (dim.w, dim.h) in
    let buf = Buffer.create 2048 in
    let rec aux l = function
      | x, y when x = w ->
          Buffer.add_char buf '\n';
          aux l (0, y + 1)
      | _, y when y = h -> ()
      | x, y -> (
          match l with
          | (hd, _) :: tl
            when Pos.sub hd { x = dim.min_x; y = dim.min_y }
                 = ({ x; y } : Pos.t) ->
              Buffer.add_char buf '#';
              aux (List.filter (fun (e, _) -> e <> hd) tl) (x + 1, y)
          | l ->
              Buffer.add_char buf '.';
              aux l (x + 1, y))
    in
    aux list (0, 0);
    Buffer.contents buf

  let main cnt =
    let rec repeat cnt ({ w; h; _ } as old_dim) time =
      let next = List.map (fun (p, v) -> (Pos.add p v, v)) cnt in
      let dim = calc_size next in
      if dim.w > w || dim.h > h then (print cnt old_dim, time)
      else repeat next dim (time + 1)
    in
    repeat cnt { w = max_int; h = max_int; min_x = 0; min_y = 0 } 0
end

module P2 = struct
  let main cnt = cnt
end

let part1 () = P1.main (cnt ()) |> fst
let part2 () = P1.main (cnt ()) |> snd |> string_of_int
