(* https://adventofcode.com/2017/day/7 *)

let cnt () =
  let parse_line l =
    let open Re.Str in
    let l, m, i = (Lib.remove_comma l, matched_group, int_of_string) in
    search_forward (regexp {|\([a-z]+\) (\([0-9]+\))\( -> \)?\(.*\)|}) l 0
    |> ignore;
    let trd =
      match m 4 l |> String.split_on_char ' ' with [ "" ] -> [] | a -> a
    in
    (m 1 l, i (m 2 l), trd)
  in
  Lib.read_file "17" "07" parse_line

module P1 = struct
  let main (cnt : (string * int * string list) list) =
    let tbl = Hashtbl.create 2048 in
    List.iter (fun (_, _, a) -> List.iter (fun x -> Hashtbl.add tbl x 0) a) cnt;
    match List.find (fun (a, _, _) -> Hashtbl.mem tbl a |> not) cnt with
    | a, _, _ -> a
end

module P2 = struct
  let rec check_tower_weight = function
    | (p1, a) :: (_, b) :: (_, c) :: _ when a <> b && b = c -> p1 - a + b
    | (_, b) :: (p1, a) :: (_, c) :: _ when a <> b && b = c -> p1 - a + b
    | (_, c) :: (_, b) :: (p1, a) :: _ when a <> b && b = c -> p1 - a + c
    | _ :: tl -> check_tower_weight tl
    | _ -> max_int

  let rec find_tower_weight tbl name =
    let w, children = Hashtbl.find tbl name in
    w + List.fold_left (fun acc e -> acc + find_tower_weight tbl e) 0 children

  let main (cnt : (string * int * string list) list) =
    let tbl = Hashtbl.create (List.length cnt) in
    List.iter (fun (a, b, c) -> Hashtbl.add tbl a (b, c)) cnt;
    let w e = fst @@ Hashtbl.find tbl e in
    let rec find_error res = function
      | [] -> res
      | (a, _, _) :: tl ->
          let children = snd (Hashtbl.find tbl a) in
          let l = List.map (fun e -> (w e, find_tower_weight tbl e)) children in
          find_error (min (check_tower_weight l) res) tl
    in
    find_error max_int cnt
end

let part1 () = P1.main (cnt ())
let part2 () = P2.main (cnt ()) |> string_of_int
