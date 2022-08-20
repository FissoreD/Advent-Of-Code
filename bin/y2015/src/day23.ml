(* https://adventofcode.com/2015/day/23 *)
open List

type memory = (int * int) * int

let operations : (string * (string * int -> memory -> memory)) list =
  [
    ( "hlf",
      fun (e, _) ((r1, r2), pos) ->
        ((if e = "a" then (r1 / 2, r2) else (r1, r2 / 2)), pos + 1) );
    ( "tpl",
      fun (e, _) ((r1, r2), pos) ->
        ((if e = "a" then (r1 * 3, r2) else (r1, r2 * 3)), pos + 1) );
    ( "inc",
      fun (e, _) ((r1, r2), pos) ->
        ((if e = "a" then (r1 + 1, r2) else (r1, r2 + 1)), pos + 1) );
    ("jmp", fun (_, off) (r, pos) -> (r, off + pos));
    ( "jie",
      fun (e, off) ((r1, r2), pos) ->
        ( (r1, r2),
          if (e = "a" && r1 mod 2 = 0) || (e = "b" && r2 mod 2 = 0) then
            pos + off
          else pos + 1 ) );
    ( "jio",
      fun (e, off) ((r1, r2), pos) ->
        ( (r1, r2),
          if (e = "a" && r1 = 1) || (e = "b" && r2 = 1) then pos + off
          else pos + 1 ) );
  ]

let cnt =
  Lib.read_file "15" "23" (fun e ->
      let l = String.split_on_char ' ' e in
      (assoc (List.hd l) operations)
        (match List.hd l with
        | x when x = "hlf" || x = "tpl" || x = "inc" -> (hd (tl l), 0)
        | y when y = "jmp" -> ("", int_of_string (hd (tl l)))
        | z when z = "jio" || z = "jie" ->
            ( hd (tl l) |> Re.Str.replace_first (Re.Str.regexp ",") "",
              int_of_string (hd (tl (tl l))) )
        | _ -> invalid_arg "Error not in list"))

let part1 () =
  let memory, stop = (((0, 0), 0), length cnt) in
  let rec aux (((_r1, r2), pos) as memory) =
    if pos >= stop then r2 else aux ((nth cnt pos) memory)
  in
  aux memory |> print_int

let part2 () =
  let memory, stop = (((1, 0), 0), length cnt) in
  let rec aux (((_r1, r2), pos) as memory) =
    if pos >= stop then r2 else aux ((nth cnt pos) memory)
  in
  aux memory |> print_int
