(* https://adventofcode.com/2015/day/24 *)

let cnt =
  Lib.read_file "15" "24" (fun e -> String.trim e |> int_of_string)
  |> List.sort compare

let total = List.fold_left ( + ) 0 cnt

module P1 = struct
  let rec sub_list tot group_weight = function
    | [] -> []
    | hd :: tl ->
        if tot + List.fold_left ( + ) 0 tl + hd < group_weight then []
        else (hd :: tl) :: sub_list tot group_weight tl

  let main group_weight =
    let min_len = ref max_int in
    let res : int list ref = ref [ max_int ] in
    let rec find_groups2 ?(acc = []) ?(tot = 0) l =
      if not (l = [] || tot > group_weight || List.length acc > !min_len) then
        List.iter
          (fun l ->
            let acc, tot, tl =
              match l with
              | [] -> (acc, tot, [])
              | hd :: tl -> (hd :: acc, tot + hd, tl)
            in
            if tot = group_weight && List.length acc <= !min_len then (
              min_len := List.length acc;
              if List.length !res = !min_len then (
                if List.fold_left ( * ) 1 !res > List.fold_left ( * ) 1 acc then
                  res := acc)
              else res := acc);
            find_groups2 ~acc ~tot tl)
          (sub_list tot group_weight l)
    in
    find_groups2 cnt;
    List.fold_left ( * ) 1 !res
end

let part1 () = P1.main (total / 3) |> string_of_int |> print_endline
let part2 () = P1.main (total / 4) |> string_of_int |> print_endline
