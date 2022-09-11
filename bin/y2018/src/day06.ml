(* https://adventofcode.com/2018/day/6 *)

let cnt () =
  Lib.read_file "18" "06" (fun l ->
      match Lib.find_all_ints_in_string l with
      | x :: [ y ] -> ({ x; y } : Pos.t)
      | _ -> failwith "Y18-D03 Invalid input")
  |> Array.of_list

module P1 = struct
  let add tbl k v =
    Hashtbl.replace tbl k (v + Option.value ~default:0 (Hashtbl.find_opt tbl k))

  let conrners (pos_list : Pos.t array) =
    let len = Array.length pos_list in
    let top_left, bot_right = (ref Pos.max_pos, ref Pos.zero) in
    for i = 0 to len - 1 do
      let a = pos_list.(i) in
      if a.x < !top_left.x then top_left := { !top_left with x = a.x };
      if a.y < !top_left.y then top_left := { !top_left with y = a.y };
      if a.x > !bot_right.x then bot_right := { !bot_right with x = a.x };
      if a.y > !bot_right.y then bot_right := { !bot_right with y = a.y }
    done;
    (!top_left, !bot_right)

  let find_nearest arr pos =
    Array.fold_left
      (fun (l, dist) e ->
        let d = Pos.air_distance D4 Square e pos in
        if d < dist then ([ e ], d)
        else if d = dist then (e :: l, d)
        else (l, dist))
      ([], max_int) arr
    |> fst

  let main cnt =
    let a, b = conrners cnt in
    let ({ x; y } : Pos.t) = Pos.add (Pos.sub b a) { x = 1; y = 1 } in
    let valid = Hashtbl.create (Array.length cnt) in
    let pos_area = Hashtbl.create (Array.length cnt) in
    Array.iteri
      (fun i p ->
        cnt.(i) <- Pos.sub p a;
        Hashtbl.add valid cnt.(i) true;
        Hashtbl.add pos_area cnt.(i) 0)
      cnt;
    for i = 0 to x - 1 do
      for j = 0 to y - 1 do
        match find_nearest cnt { x = i; y = j } with
        | [] -> invalid_arg "Y18-D6 Error"
        | [ a ] ->
            if i = x - 1 || j = y - 1 || i = 0 || j = 0 then
              Hashtbl.replace valid a false;
            Hashtbl.replace pos_area a (1 + Hashtbl.find pos_area a)
        | _ -> ()
      done
    done;
    Hashtbl.fold (fun k v acc -> if v then k :: acc else acc) valid []
    |> List.map (Hashtbl.find pos_area)
    |> List.fold_left max 0
end

module P2 = struct
  let sum_of_all l p =
    Array.fold_left (fun acc p' -> acc + Pos.air_distance D4 Square p p') 0 l

  let main ~total cnt =
    let a, b = P1.conrners cnt in
    let res = ref 0 in
    let padding = total / Array.length cnt in
    for i = a.x - padding to b.x + padding do
      for j = a.y - padding to b.y + padding do
        if sum_of_all cnt { x = i; y = j } < total then incr res
      done
    done;
    !res
end

(* Not 4885 (too high) *)
let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main ~total:10_000 (cnt ()) |> string_of_int
