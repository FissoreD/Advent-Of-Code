(* https://adventofcode.com/2018/day/17 *)

let cnt () =
  let top_left, bot_right = (ref Pos.max_pos, ref Pos.zero) in
  let parser l =
    let ints = Lib.find_all_ints_in_string l in
    let ((x1, x2, y1, y2) as coords) =
      if l.[0] = 'x' then List.(hd ints, hd ints, nth ints 1, nth ints 2)
      else List.(nth ints 1, nth ints 2, hd ints, hd ints)
    in
    top_left := { x = min !top_left.x x1; y = min !top_left.y y1 };
    bot_right := { x = max !bot_right.x x2; y = max !bot_right.y y2 };
    coords
  in
  let parsed = Lib.read_file "18" "17" parser in
  (parsed, !top_left, !bot_right)

module P1 = struct
  module Set = Set.Make (Pos)

  let get_grid_cnt grid (pos : Pos.t) = grid.(pos.y).(pos.x)

  let count_occurences grid char =
    Array.fold_left
      (fun acc e ->
        Array.fold_left
          (fun acc e -> acc + if e = Some char then 1 else 0)
          acc e)
      0 grid

  let replace_vertical is_left grid pos =
    let pos = Pos.move_in_square pos (if is_left then E else W) in
    let rec aux (pos : Pos.t) =
      if
        pos.x < Array.length grid.(0)
        && pos.x >= 0
        && grid.(pos.y).(pos.x) = Some '~'
      then (
        grid.(pos.y).(pos.x) <- Some '|';
        aux (Pos.move_in_square pos (if is_left then E else W)))
    in
    aux pos

  let rec fill_horizontally grid is_left pos counter =
    let pos = Pos.move_in_square pos (if is_left then W else E) in
    let down = Pos.move_in_square pos S in
    if grid.(down.y).(down.x) = Some '.' then pos
    else
      match grid.(pos.y).(pos.x) with
      | None -> Pos.max_pos
      | Some (('.' | '~' | '|') as c) ->
          if c = '.' then incr counter;
          grid.(pos.y).(pos.x) <- Some '~';
          fill_horizontally grid is_left pos counter
      | Some _ -> Pos.max_pos

  let rec fill_down grid (pos : Pos.t) counter =
    if pos.y < Array.length grid then
      match grid.(pos.y).(pos.x) with
      | Some '.' ->
          grid.(pos.y).(pos.x) <- Some '|';
          incr counter;
          fill_down grid (Pos.move_in_square pos S) counter
      | None ->
          let new_pos = Pos.move_in_square pos N in
          grid.(new_pos.y).(new_pos.x) <- Some '~';
          new_pos
      | Some '~' ->
          let left = fill_horizontally grid true pos counter in
          let right = fill_horizontally grid false pos counter in
          if left <> Pos.max_pos || right <> Pos.max_pos then Pos.max_pos
          else
            let new_pos = Pos.move_in_square pos N in
            if get_grid_cnt grid new_pos = Some '.' then incr counter;
            grid.(new_pos.y).(new_pos.x) <- Some '~';
            new_pos
      | _ -> Pos.max_pos
    else Pos.max_pos

  let rec run (pos : Pos.t) grid counter =
    let down_is_free (p : Pos.t) =
      p <> Pos.max_pos && grid.(p.y + 1).(p.x) = Some '.'
    in
    if pos.y < Array.length grid then
      let pos1 = fill_down grid pos counter in
      if pos1 <> Pos.max_pos then
        let left = fill_horizontally grid true pos1 counter in
        let right = fill_horizontally grid false pos1 counter in
        if left <> Pos.max_pos || right <> Pos.max_pos then (
          let should_go_down pos dir =
            down_is_free pos
            && get_grid_cnt grid (Pos.move_in_square pos dir) <> Some '|'
          in
          replace_vertical true grid left;
          replace_vertical false grid right;
          if should_go_down left SE then run left grid counter;
          if should_go_down right SW then run right grid counter)
        else run pos1 grid counter
      else (
        replace_vertical true grid pos;
        replace_vertical false grid pos)

  let make_grid (coords, (tl : Pos.t), (br : Pos.t)) =
    let h, w = (br.y - tl.y + 2, br.x - tl.x + 3) in
    let grid = Array.make_matrix h w (Some '.') in
    grid.(0).(500 - tl.x + 1) <- Some '+';
    List.iter
      (fun (x1, x2, y1, y2) ->
        for x = x1 to x2 do
          for y = y1 to y2 do
            grid.(y - tl.y + 1).(x - tl.x + 1) <- None
          done
        done)
      coords;
    (grid, tl)

  let main cnt =
    let grid, top_left = make_grid cnt in
    let counter = ref 0 in
    run { x = 500 - top_left.x + 1; y = 1 } grid counter;
    !counter
end

module P2 = struct
  open P1

  let main cnt =
    let grid, top_left = make_grid cnt in
    run { x = 500 - top_left.x + 1; y = 1 } grid (ref 0);
    count_occurences grid '~'
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
