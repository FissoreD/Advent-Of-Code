(* https://adventofcode.com/2016/day/21 *)
let swap arr i1 i2 =
  let x = arr.(i1) in
  arr.(i1) <- arr.(i2);
  arr.(i2) <- x;
  arr

let rotate is_left arr step =
  let my_if a b = if is_left then a else b in
  let len = Array.length arr in
  let step = step mod len in
  let start_pos = my_if 0 @@ (len - step) in
  let store = Array.sub arr start_pos step in
  for i = my_if step 1 to len - my_if 1 step do
    arr.(my_if (i - step) (len - i)) <- arr.(my_if i (len - i - step))
  done;
  Array.iteri (fun i e -> arr.(my_if (len - step + i) i) <- e) store;
  arr

let duplicate f = (lazy f, lazy f)

let rotate_lazy b arr step =
  (lazy (rotate b arr step), lazy (rotate (not b) arr step))

let move_position arr i1 i2 =
  let old = arr.(i1) in
  if i1 < i2 then
    for i = i1 to i2 - 1 do
      arr.(i) <- arr.(i + 1)
    done
  else
    for i = i1 downto i2 + 1 do
      arr.(i) <- arr.(i - 1)
    done;
  arr.(i2) <- old;
  arr

let rec instr =
  [
    ("swap position", fun (i1, i2, _, _) arr -> duplicate @@ swap arr i1 i2);
    ( "swap letter",
      fun (_, _, l1, l2) arr ->
        let p1, p2 = Lib.Array.(findi (( = ) l1) arr, findi (( = ) l2) arr) in
        duplicate @@ swap arr p1 p2 );
    ("rotate left", fun (step, _, _, _) arr -> rotate_lazy true arr step);
    ("rotate right", fun (step, _, _, _) arr -> rotate_lazy false arr step);
    ( "rotate based",
      fun ((_, _, l, _) as x) arr ->
        ( lazy
            (rotate false arr
               (let i = Lib.Array.findi (( = ) l) arr in
                i + 1 + Lib.bool_to_int (i >= 4))),
          lazy
            (let arr_clone arr =
               Array.init (Array.length arr) (fun i -> arr.(i))
             in
             let rec aux arr1 =
               let clone = arr_clone arr1 in
               let res =
                 (List.assoc "rotate based" instr) x clone |> fst |> Lazy.force
               in
               if Array.for_all2 ( = ) arr res then arr1
               else aux (rotate true arr1 1)
             in
             aux (arr_clone arr)) ) );
    ( "reverse positions",
      fun (i1, i2, _, _) arr ->
        duplicate
          (let copy = Array.sub arr i1 (i2 - i1 + 1) in
           Array.iteri (fun pos e -> arr.(i2 - pos) <- e) copy;
           arr) );
    ( "move position",
      fun (i1, i2, _, _) arr ->
        (lazy (move_position arr i1 i2), lazy (move_position arr i2 i1)) );
  ]

let cnt =
  let open List in
  let i g = try int_of_string g with _ -> -1 in
  let func l =
    let l = String.split_on_char ' ' l in
    let fst = hd l ^ " " ^ hd (tl l) in
    let get_i a b =
      let a, b = (nth l a, nth l b) in
      assoc fst instr (i a, i b, a.[0], b.[0])
    in
    match fst with
    | "swap position" | "move position" | "swap letter" -> get_i 2 5
    | "reverse positions" -> get_i 2 4
    | "rotate right" | "rotate left" -> get_i 2 2
    | "rotate based" -> get_i 6 6
    | _ -> invalid_arg "Invalid input"
  in
  Lib.read_file "16" "21" func

module P1 = struct
  let string_to_char_array str =
    Array.init (String.length str) (fun i -> str.[i])

  let main str =
    let arr = string_to_char_array str in
    List.fold_left (fun acc e -> Lazy.force (fst (e acc))) arr cnt
end

module P2 = struct
  let main str =
    let arr = P1.string_to_char_array str in
    List.fold_left (fun acc e -> Lazy.force (snd (e acc))) arr (List.rev cnt)
end

let part1 () =
  P1.main "abcdefgh" |> Array.iter print_char;
  print_endline ""

let part2 () =
  P2.main "fbgdceah" |> Array.iter print_char;
  print_endline ""
