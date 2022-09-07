(* https://adventofcode.com/2017/day/19 *)

let cnt () = Lib.read_file "17" "19" Fun.id |> Array.of_list

type dir = Pos.T.t

module P1 = struct
  let start_pos cnt = Re.Str.(search_forward (regexp "|") cnt.(0) 0)

  let get_char w h map pos =
    if Pos.is_valid (w, h) pos then map.(pos.y).[pos.x] else ' '

  let is_letter c =
    Lib.in_bouond_inclusive (int_of_char 'A', int_of_char 'Z') (int_of_char c)

  let walk_path cnt =
    let w, h = (String.length cnt.(0), Array.length cnt) in
    let get_char = get_char w h cnt in
    let rec aux pos dir (w, len) =
      let p = Pos.move_in_square pos dir in
      match get_char p with
      | '+' when dir = N || dir = S -> (
          match get_char (Pos.move_in_square p W) with
          | l when l = '-' || is_letter l -> aux p W (w, len + 1)
          | _ -> aux p E (w, len + 1))
      | '+' -> (
          match get_char (Pos.move_in_square p S) with
          | l when l = '|' || is_letter l -> aux p S (w, len + 1)
          | _ -> aux p N (w, len + 1))
      | l when is_letter l -> aux p dir (l :: w, len + 1)
      | ' ' -> (w |> List.rev_map Lib.string_of_char |> String.concat "", len)
      | _ -> aux p dir (w, len + 1)
    in
    aux { x = start_pos cnt; y = 0 } S ([], 1)

  let main cnt = walk_path cnt |> fst
end

module P2 = struct
  let main cnt = P1.walk_path cnt |> snd
end

let part1 () = P1.main (cnt ())
let part2 () = P2.main (cnt ()) |> string_of_int
