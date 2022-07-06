open Lib

let inp () =
  read_file "2019" "day02" (fun e ->
      String.split_on_char ',' e |> List.map int_of_string)
  |> List.hd

let rec calc_one arr posIn =
  if arr.(posIn) = 99 then arr
  else
    match (arr.(posIn), arr.(posIn + 1), arr.(posIn + 2), arr.(posIn + 3)) with
    | 1, a, b, pos ->
        arr.(pos) <- arr.(a) + arr.(b);
        calc_one arr (posIn + 4)
    | 2, a, b, pos ->
        arr.(pos) <- arr.(a) * arr.(b);
        calc_one arr (posIn + 4)
    | _ -> raise Invalid_input

let day02_1 () =
  let i = inp () in
  let arr = Array.of_list i in
  arr.(1) <- 12;
  arr.(2) <- 2;
  calc_one arr 0

(* let _ = (day02_1 ()).(0) |> Printf.printf "%d\n" *)

let all_combo a b =
  let rec aux1 = function
    | a1, _ when a1 = a -> []
    | a1, b1 when b1 = b -> aux1 (a1 + 1, 0)
    | a1, b1 -> (a1, b1) :: aux1 (a1, b1 + 1)
  in
  aux1 (0, 0)

let rec find_comb l combo =
  let arr = Array.of_list l in
  match combo with
  | (a, b) :: tl ->
      arr.(1) <- a;
      arr.(2) <- b;
      if try (calc_one arr 0).(0) = 19690720 with _ -> false then (a * 100) + b
      else find_comb l tl
  | _ -> invalid_arg "Error"

let day02_2 () = find_comb (inp ()) (all_combo 100 100)

let _ = day02_2 () |> Printf.printf "%d\n"
