(* https://adventofcode.com/2018/day/9 *)

let cnt () =
  match Lib.read_file "18" "09" Lib.find_all_ints_in_string |> List.hd with
  | a :: [ b ] -> (a, b)
  | _ -> invalid_arg "Y18-D9 invalid input"

module P1 = struct
  type t = { mutable next : t; value : int; mutable prev : t }

  let create value =
    let rec res = { next = res; value; prev = res } in
    res

  let add_after value cnt =
    let new_cell = { next = cnt.next; value; prev = cnt } in
    cnt.next.prev <- new_cell;
    cnt.next <- new_cell;
    new_cell

  let remove_cell cnt =
    cnt.prev.next <- cnt.next;
    cnt.next.prev <- cnt.prev;
    cnt.next

  let go_back_7 cnt =
    let rec aux cnt = function 7 -> cnt | n -> aux cnt.prev (n + 1) in
    aux cnt 0

  let main ~is_part1 cnt =
    let players, max_points = cnt in
    let max_points = max_points * if is_part1 then 1 else 100 in
    let stats = Array.make players 0 in
    let rec aux next23mul current = function
      | n when n = max_points + 1 -> Array.fold_left max 0 stats
      | n when n = next23mul ->
          let back = go_back_7 current in
          let player_ind = (n - 1) mod players in
          stats.(player_ind) <- stats.(player_ind) + n + back.value;
          aux (next23mul + 23) (remove_cell back) (n + 1)
      | n -> aux next23mul (add_after n current.next) (n + 1)
    in
    aux 23 (create 0) 1
end

let part1 () = P1.main ~is_part1:true (cnt ()) |> string_of_int
let part2 () = P1.main ~is_part1:false (cnt ()) |> string_of_int