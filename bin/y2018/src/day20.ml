(* https://adventofcode.com/2018/day/20 *)

let cnt () = Lib.read_file "18" "20" Fun.id |> List.hd

module P1 = struct
  let furthest grid =
    let module Set = Set.Make (Pos) in
    let w, h = (Array.length grid.(0), Array.length grid) in
    let explored = Hashtbl.create (w * h) in
    let is_valid (pos : Pos.t) =
      (not @@ Hashtbl.mem explored pos) && grid.(pos.y).(pos.x) <> '#'
    in
    let rec aux pos_l dist =
      match pos_l with
      | x when x <> Set.empty ->
          let next =
            Set.fold
              (fun e acc ->
                if is_valid e then (
                  Hashtbl.add explored e dist;
                  List.fold_left
                    (fun acc e -> if is_valid e then Set.add e acc else acc)
                    acc (Pos.neighbors e))
                else acc)
              pos_l Set.empty
          in
          aux next (dist + 1)
      | _ -> (dist / 2, explored)
    in
    let middle = Array.length grid / 2 in
    aux (Set.singleton { x = middle; y = middle }) 0

  let make_world size cnt =
    let world = Array.make_matrix size size '#' in
    let regex = Day20_parser.f Day20_lexer.f (Lexing.from_string cnt) in
    let rec aux pos (next : Day20_types.regex) = function
      | Day20_types.Empty when next <> Empty -> aux pos Empty next
      | Symbol d ->
          let new_pos = Pos.move_in_square pos d in
          world.(new_pos.y).(new_pos.x) <- '.';
          let new_pos = Pos.move_in_square new_pos d in
          world.(new_pos.y).(new_pos.x) <- '.';
          aux new_pos Empty next
      | Cat (a, b) -> aux pos b a
      | Choice (a, b) ->
          aux pos next a;
          aux pos next b
      | _ -> ()
    in
    world.(size / 2).(size / 2) <- 'X';
    aux { x = size / 2; y = size / 2 } Empty regex;
    world

  let main cnt = furthest (make_world 300 cnt) |> fst
end

module P2 = struct
  open P1

  let main cnt =
    let min_dist = 1000 in
    let distances = furthest (make_world 300 cnt) |> snd in
    let add v = Lib.bool_to_int (v land 1 = 0 && v / 2 >= min_dist) in
    Hashtbl.fold (fun _ v acc -> acc + add v) distances 0
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
