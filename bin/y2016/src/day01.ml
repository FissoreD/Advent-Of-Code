(* https://adventofcode.com/2016/day/1 *)

let cnt () =
  Lib.read_file "16" "01" (Re.Str.split (Re.Str.regexp ", "))
  |> List.hd
  |> List.map (fun e ->
         (String.get e 0, int_of_string (String.sub e 1 (String.length e - 1))))

type dir = { x : int; y : int }
type pos = { x : int; y : int; dir : dir }

module P1 = struct
  let dist { x; y; _ } = abs x + abs y

  let dirs =
    [ { x = 1; y = 0 }; { x = 0; y = -1 }; { x = -1; y = 0 }; { x = 0; y = 1 } ]

  let next_pos { x; y; dir } (d, dist) =
    let dir =
      List.nth dirs
        ((4 + Lib.List.index_of dir dirs + if d = 'L' then -1 else 1) mod 4)
    in
    { x = x + (dir.x * dist); y = y + (dir.y * dist); dir }

  let main cnt =
    List.fold_left next_pos { x = 0; y = 0; dir = { x = 0; y = 1 } } cnt |> dist
end

module P2 = struct
  let same_pos p1 p2 = p1.x = p2.x && p1.y = p2.y

  let rec intermediate_pos ?(acc = []) (p1 : pos) (p2 : pos) list_hd =
    if same_pos p1 p2 then
      if same_pos p1 list_hd then p1 :: Lib.List.remove_last acc
      else List.rev acc
    else if p1.x > p2.x || p1.y > p2.y then intermediate_pos ~acc p2 p1 list_hd
    else
      intermediate_pos ~acc:(p1 :: acc)
        (if p1.y = p2.y then { p1 with x = p1.x + 1; y = p1.y }
        else { p1 with x = p1.x; y = p1.y + 1 })
        p2 list_hd

  let main cnt =
    let rec aux acc_list pos l =
      let intermediate, acc_list =
        if acc_list = [] then ([], [ pos ])
        else (intermediate_pos pos (List.hd acc_list) pos, acc_list)
      in
      match
        List.find_opt (fun p -> List.exists (same_pos p) intermediate) acc_list
      with
      | None ->
          if l = [] then P1.dist pos
          else
            let next_pos = P1.next_pos pos (List.hd l) in
            aux (intermediate @ acc_list) next_pos (List.tl l)
      | Some pos -> P1.dist pos
    in
    aux [] { x = 0; y = 0; dir = { x = 0; y = 1 } } cnt
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int