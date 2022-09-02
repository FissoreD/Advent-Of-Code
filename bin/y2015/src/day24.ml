(* https://adventofcode.com/2015/day/24 *)

let cnt () =
  Lib.read_file "15" "24" (fun e -> String.trim e |> int_of_string)
  |> List.sort compare

let rec sub_list tot group_weight = function
  | [] -> []
  | _ :: tl as l -> l :: sub_list tot group_weight tl

let smallest_product a b = List.fold_left ( * ) 1 a > List.fold_left ( * ) 1 b

let update_acc_tot acc tot l =
  if l = [] then (acc, tot, []) else List.(hd l :: acc, tot + hd l, tl l)

let main cnt =
  let group_weight = List.fold_left ( + ) 0 cnt in
  let min_len, res = (ref max_int, ref [ max_int ]) in
  let rec find_groups2 ?(acc = []) ?(tot = 0) l =
    if not (l = [] || tot > group_weight || List.length acc > !min_len) then
      List.iter
        (fun l ->
          let acc, tot, tl = update_acc_tot acc tot l in
          if tot = group_weight && List.length acc <= !min_len then (
            min_len := List.length acc;
            if List.length !res = !min_len then (
              if smallest_product !res acc then res := acc)
            else res := acc);
          find_groups2 ~acc ~tot tl)
        (sub_list tot group_weight l)
  in
  find_groups2 cnt;
  List.fold_left ( * ) 1 !res

let part1 () = main (cnt ()) |> string_of_int |> print_endline
let part2 () = main (cnt ()) |> string_of_int |> print_endline
