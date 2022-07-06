open Lib

let parse_line e = (e.[0], int_of_string @@ String.sub e 1 (String.length e - 1))

let inp () =
  read_file "2019" "day03" (fun e ->
      String.split_on_char ',' e |> List.map parse_line)

let distance (a, b) (c, d) = abs (a - c) + abs (b - d)

let calc_one_line l =
  List.fold_left
    (fun l (c, i) ->
      match l with
      | (x, y) :: tl ->
          (match c with
          | 'U' -> (x, y + i)
          | 'D' -> (x, y - i)
          | 'R' -> (x + i, y)
          | 'L' -> (x - i, y)
          | _ -> invalid_arg "Invalid char")
          :: l
      | _ -> raise Invalid_input)
    [ (0, 0) ] l

let rec intersect l elt = match l with h1 :: h2 :: tl -> true | _ -> false

let day03_1 () =
  let res = List.map calc_one_line (inp ()) in
  match res with
  | a :: b :: tl ->
      let a, b =
        List.filter (intersect b) a
        |> List.fold_left
             (fun a b ->
               let a' = distance a (0, 0) in
               let b' = distance b (0, 0) in
               if min a' b' = a' && a' > 0 then a else b)
             (Int.max_int - 500, Int.max_int - 500)
      in
      a + b
  | _ -> raise Invalid_input

let _ =
  day03_1 () |> print_int;
  print_newline ()
