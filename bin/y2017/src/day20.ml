(* https://adventofcode.com/2017/day/20 *)

let cnt () =
  Lib.read_file "17" "20" Lib.find_all_ints_in_string
  |> List.map (List.map float)
  |> Array.of_list

module P1 = struct
  let calc_pos n = function
    | [ x; y; z; vx; vy; vz; ax; ay; az ] ->
        ( x +. ((vx +. (ax /. 2.)) *. n) +. (ax /. 2. *. n *. n),
          y +. ((vy +. (ay /. 2.)) *. n) +. (ay /. 2. *. n *. n),
          z +. ((vz +. (az /. 2.)) *. n) +. (az /. 2. *. n *. n) )
    | _ -> failwith "Y17-D20 := Invalid input"

  let distance (a, b, c) = Float.(abs a +. abs b +. abs c)

  let main (cnt : float list array) =
    let arr = cnt |> Array.map (calc_pos 1_000_000.) |> Array.map distance in
    let min_value, min_pos = (ref infinity, ref 0) in
    for i = 0 to Array.length arr - 1 do
      if arr.(i) < !min_value then (
        min_value := arr.(i);
        min_pos := i)
    done;
    !min_pos
end

module P2 = struct
  let is_int f = f = Float.round f

  let find_intersection l1 l2 =
    match List.map2 ( -. ) l1 l2 with
    | [ x; _; _; vx; _; _; ax; _; _ ] ->
        let a = ax /. 2. in
        let b = (ax /. 2.) +. vx in
        let c = x in
        let res =
          if a = 0. && b = 0. then [ c ]
          else if a = 0. then [ -.c /. b ]
          else
            let delta = (b *. b) -. (4. *. a *. c) in
            if delta < 0. then []
            else
              let root_f op = op (-.b) (sqrt delta) /. (2. *. a) in
              [ root_f ( +. ); root_f ( -. ) ]
        in
        let filter_cond c =
          is_int c && c >= 0. && P1.calc_pos c l1 = P1.calc_pos c l2
        in
        List.filter filter_cond res |> List.fold_left min infinity
    | _ -> failwith "Error"

  let main (cnt : float list array) =
    let len = Array.length cnt in
    let tbl = Hashtbl.create len in
    let remaining_part = Array.init len (fun i -> ref (Some i)) in
    for p1 = 0 to len - 1 do
      for p2 = p1 + 1 to len - 1 do
        let res = find_intersection cnt.(p1) cnt.(p2) in
        let old = Hashtbl.find_opt tbl res in
        Hashtbl.replace tbl res
          (remaining_part.(p1) :: remaining_part.(p2)
          :: Option.value ~default:[] old)
      done
    done;
    let distancies =
      List.of_seq (Hashtbl.to_seq_keys tbl)
      |> List.sort compare
      |> List.filter (( <> ) infinity)
    in
    let module Set = Set.Make (Int) in
    List.iter
      (fun (e : float) ->
        let l = Hashtbl.find tbl e in
        let not_null = List.filter_map (fun e -> !e) l |> Set.of_list in
        if Set.cardinal not_null > 1 then
          Set.iter (fun e -> remaining_part.(e) := None) not_null)
      distancies;
    Array.fold_left
      (fun acc e -> acc + if !e = None then 0 else 1)
      0 remaining_part
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
