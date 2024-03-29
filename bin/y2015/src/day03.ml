(* https://adventofcode.com/2015/day/3 *)

module MySet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let cnt () =
  match Lib.read_file "15" "03" Lib.string_2_char_list with
  | [ l ] -> l
  | _ -> raise Lib.Invalid_input

module P1 = struct
  let move (x, y) = function
    | '^' -> (x, y + 1)
    | 'v' -> (x, y - 1)
    | '<' -> (x - 1, y)
    | '>' -> (x + 1, y)
    | _ -> raise Lib.Invalid_input

  let add_move (set, pos) f =
    let newPos = move pos f in
    (MySet.add newPos set, newPos)

  let makeMove cnt =
    let set = MySet.singleton (0, 0) in
    let pos = (0, 0) in
    List.fold_left add_move (set, pos) cnt |> fst
end

module P2 = struct
  let move_alternate cnt =
    let l1 = List.filteri (fun a _ -> a mod 2 = 0) cnt in
    let l2 = List.filteri (fun a _ -> a mod 2 <> 0) cnt in
    let set1, set2 = (P1.makeMove l1, P1.makeMove l2) in
    MySet.(union set1 set2 |> cardinal)
end

let part1 () = P1.makeMove (cnt ()) |> MySet.cardinal |> string_of_int
let part2 () = P2.move_alternate (cnt ()) |> string_of_int
