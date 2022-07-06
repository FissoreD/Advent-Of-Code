open Lib

let inp () =
  read_file "day12" (fun x ->
      (x.[0], String.sub x 1 (String.length x - 1) |> int_of_string))

let rec turn_cap ((a, b) as cap) = function
  | 0 -> cap
  | n -> turn_cap (b * -1, a) (n - 90)

let modif_pos ((x, y) as pos) ((a, b) as cap) (l, n) =
  match l with
  | 'F' -> (pos ++ (cap ** (n, n)), cap)
  | 'E' -> ((x, y + n), cap)
  | 'W' -> ((x, y - n), cap)
  | 'N' -> ((x + n, y), cap)
  | 'S' -> ((x - n, y), cap)
  | 'R' -> (pos, turn_cap cap (ang_in_360 n))
  | _ -> (pos, turn_cap cap (ang_compl n))

let rec calc (pos, cap) = function
  | [] -> (pos, cap)
  | hd :: tl -> calc (modif_pos pos cap hd) tl

let day12_1 () =
  match inp () |> calc ((0, 0), (0, 1)) with (a, b), _ -> abs (a + b)

let modif_pos' ((x, y) as pos) ((a, b) as cap) (l, n) =
  match l with
  | 'F' -> (pos ++ (cap ** (n, n)), cap)
  | 'E' -> (pos, (a, b + n))
  | 'W' -> (pos, (a, b - n))
  | 'N' -> (pos, (a + n, b))
  | 'S' -> (pos, (a - n, b))
  | 'R' -> (pos, turn_cap cap (ang_in_360 n))
  | _ -> (pos, turn_cap cap (ang_compl n))

let rec calc' (pos, cap) = function
  | [] -> (pos, cap)
  | hd :: tl -> calc' (modif_pos' pos cap hd) tl

let day12_2 () =
  match inp () |> calc' ((0, 0), (1, 10)) with (a, b), _ -> abs (a + b)
