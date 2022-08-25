(* https://adventofcode.com/2016/day/12 *)
open Lib.List

type memory = { a : int; b : int; c : int; d : int; i : int }

let operations =
  let incr mem = { mem with i = mem.i + 1 } in
  let get m = function "a" -> m.a | "b" -> m.b | "c" -> m.c | _ -> m.d in
  let ternary_opt mem v = if Lib.is_int v then int_of_string v else get mem v in
  let set mem v = function
    | "a" -> { mem with a = v }
    | "b" -> { mem with b = v }
    | "c" -> { mem with c = v }
    | _ -> { mem with d = v }
  in
  [
    ("cpy", fun (_, r, src) mem -> incr (set mem (ternary_opt mem src) r));
    ("inc", fun (_, r, _) mem -> incr (set mem (get mem r + 1) r));
    ("dec", fun (_, r, _) mem -> incr (set mem (get mem r - 1) r));
    ( "jnz",
      fun (s, _, src) mem ->
        let res = ternary_opt mem src in
        if res = 0 then incr mem else { mem with i = mem.i + s } );
  ]

let cnt =
  Lib.read_file "16" "12" (fun e ->
      let l = String.split_on_char ' ' e in
      (assoc (List.hd l) operations)
        (match l with
        | "inc" :: [ r ] | "dec" :: [ r ] -> (-1, r, "")
        | "cpy" :: a :: [ r ] -> (-1, r, a)
        | "jnz" :: r :: [ s ] -> (int_of_string s, "", r)
        | _ -> invalid_arg "Error not in list"))

let part1 ?(m = { a = 0; b = 0; c = 0; d = 0; i = 0 }) () =
  let alt = length cnt in
  let rec aux m = if m.i >= alt then m.a else aux ((nth cnt m.i) m) in
  aux m |> print_int

let part2 ?(m = { a = 0; b = 0; c = 1; d = 0; i = 0 }) = part1 ~m
