(* https://adventofcode.com/2016/day/11 *)
open Re.Str
open Printf

type row = { gen : string list; chip : string list }
type diagram_type = row list
type memory = { elev_pos : int; step_nb : int; diagram : diagram_type }

let empty = { gen = []; chip = [] }
let row_concat r1 r2 = { gen = r1.gen @ r2.gen; chip = r1.chip @ r2.chip }

let make_row a is_c =
  if is_c then { chip = a; gen = [] } else { chip = []; gen = a }

(* Chip 'a is fired if it is with a ~'a generator && without the 'a generator *)
let cnt =
  let regex = regexp {|\([^- ]*\)\(-[^ ]*\)? \([^ ]*\)|} in
  let parse_line l =
    Re.Str.global_replace (Re.Str.regexp {|\(,\|\.\)|}) "" l
    |> split (regexp " a ")
    |> List.tl
    |> List.map (fun e ->
           search_forward regex e 0 |> ignore;
           let m1, m3 = ([ matched_group 1 e ], matched_group 3 e) in
           make_row m1 (m3 = "microchip"))
    |> List.fold_left row_concat empty
  in
  Lib.read_file "16" "11" parse_line |> List.rev

module P1 = struct
  open Lib.List

  let get_elt { chip; gen } is_chip = if is_chip then chip else gen

  let prev_floors_are_empty diagram pos =
    let is_empty, pos = (ref true, ref (pos + 1)) in
    while !pos < length diagram && !is_empty do
      if nth diagram !pos <> empty then is_empty := false;
      incr pos
    done;
    !is_empty

  let is_valid_row { chip; gen } =
    for_all (fun x -> mem x gen || length gen = 0) chip

  let movable_object row =
    let rec singleton_list { gen; chip } =
      let make_op t1 t2 elt = elt :: singleton_list { gen = t1; chip = t2 } in
      match (gen, chip) with
      | [], [] -> []
      | h1 :: t1, t2 -> make_op t1 t2 (make_row [ h1 ] false)
      | t1, h1 :: t2 -> make_op t1 t2 (make_row [ h1 ] true)
    in
    let rec couple_list { gen; chip } =
      let make_op =
        let next f gen chip = f { gen; chip } in
        let compose h1 t1 l add_gen =
          map
            (fun { gen; chip } ->
              if add_gen then { gen = h1 :: gen; chip }
              else { gen; chip = h1 :: chip })
            (next singleton_list t1 l)
          @ next couple_list t1 l
        in
        compose
      in
      match (gen, chip) with
      | [], [] | [ _ ], [] | [], [ _ ] -> []
      | h1 :: t1, l -> make_op h1 t1 l true
      | t1, h1 :: l -> make_op h1 t1 l false
    in
    singleton_list row @ couple_list row

  let next_conf { elev_pos; diagram; _ } movable current_row next_pos =
    let new_current_row =
      let not_mem is_chip e = not (mem e (get_elt movable is_chip)) in
      let filter is_c = filter (not_mem is_c) (get_elt current_row is_c) in
      { chip = filter true; gen = filter false }
    in
    let new_next_row = row_concat (nth diagram next_pos) movable in
    let give_row pos e =
      if pos = elev_pos then new_current_row
      else if pos = next_pos then new_next_row
      else e
    in
    if is_valid_row new_current_row && is_valid_row new_next_row then
      Some (mapi give_row diagram, next_pos)
    else None

  let sum_lengths { chip; gen } = length chip + length gen

  let not_explored elev_pos diagram (e_pos1, d1) =
    let diff_len a b is_chip =
      length (get_elt a is_chip) <> length (get_elt b is_chip)
    in
    let same_length a b = diff_len a b false || diff_len a b true in
    e_pos1 <> elev_pos || exists2 same_length d1 diagram

  let main ?(cnt = cnt) () =
    let height = length cnt - 1 in
    let materials_nb = map sum_lengths cnt |> fold_left ( + ) 0 in
    let has_finished diagram = sum_lengths @@ hd diagram = materials_nb in
    let to_analyse, already_explored = (Queue.create (), ref []) in
    Queue.add { elev_pos = height; diagram = cnt; step_nb = 0 } to_analyse;
    let res = ref (-1) in
    while !res = -1 do
      let { elev_pos; diagram; step_nb } = Queue.pop to_analyse in
      if has_finished diagram then res := step_nb
      else if for_all (not_explored elev_pos diagram) !already_explored then (
        already_explored := (elev_pos, diagram) :: !already_explored;
        let current_row = nth diagram elev_pos in
        let movables = movable_object current_row in
        let new_pos_list =
          (if prev_floors_are_empty diagram elev_pos then [ elev_pos - 1 ]
          else [ elev_pos + 1; elev_pos - 1 ])
          |> filter (Lib.in_bouond_inclusive (0, height))
        in
        let add_to_analyse (diagram, elev_pos) =
          Queue.add { elev_pos; diagram; step_nb = step_nb + 1 } to_analyse
        in
        let double_iter a b f = iter (fun x -> iter (fun y -> f x y) b) a in
        double_iter movables new_pos_list (fun movable next_pos ->
            let next_conf =
              next_conf
                { diagram; step_nb; elev_pos }
                movable current_row next_pos
            in
            if Option.is_some next_conf then
              add_to_analyse (Option.get next_conf)))
    done;
    !res
end

module P2 = struct
  let couple = [ "elerium"; "dilithium" ]

  let build_new_cnt () =
    match List.rev cnt with
    | hd :: tl -> List.rev (row_concat hd { gen = couple; chip = couple } :: tl)
    | _ -> raise Exit

  let main = P1.main ~cnt:(build_new_cnt ())
end

let part1 () = P1.main () |> printf "%d\n"
let part2 () = P2.main () |> printf "%d\n"
