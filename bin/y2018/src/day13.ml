(* https://adventofcode.com/2018/day/13 *)

type car = { dir_cnt : int; dir : Pos.Dir.dirs; pos : Pos.t; id : int }

let cnt () =
  Lib.read_file "18" "13" (fun e -> Lib.string_2_char_list e |> Array.of_list)
  |> Array.of_list

module P1 = struct
  let id_gen () =
    let id = ref (-1) in
    fun () ->
      incr id;
      !id

  let parse_input inp =
    let cars, tbl = (Queue.create (), Hashtbl.create 2048) in
    let id = id_gen () in
    let add pos dir =
      let id = id () in
      let car = { dir_cnt = 0; dir; pos; id } in
      Queue.add car cars;
      Hashtbl.add tbl pos car
    in
    for y = 0 to Array.length inp - 1 do
      for x = 0 to Array.length inp.(0) - 1 do
        let (p : Pos.t) = { x; y } in
        match inp.(y).(x) with
        | '^' -> add p N
        | '>' -> add p E
        | 'v' -> add p S
        | '<' -> add p W
        | _ -> ()
      done
    done;
    (cars, tbl)

  let next_dir_inters { dir_cnt; dir; pos; id } =
    let dir_cnt = dir_cnt mod 3 in
    let left = Pos.Dir.(function N -> W | W -> S | S -> E | _ -> N) in
    let dir =
      [| left; Fun.id; (fun e -> left (left (left e))) |].(dir_cnt) dir
    in
    { dir; dir_cnt = dir_cnt + 1; pos; id }

  let move ({ pos; dir; _ } as car) lab =
    let car =
      match lab.(pos.y).(pos.x) with
      | '+' -> next_dir_inters car
      | '\\' when dir = N -> { car with dir = W }
      | '\\' when dir = S -> { car with dir = E }
      | '\\' when dir = E -> { car with dir = S }
      | '\\' when dir = W -> { car with dir = N }
      | '/' when dir = W -> { car with dir = S }
      | '/' when dir = S -> { car with dir = W }
      | '/' when dir = N -> { car with dir = E }
      | '/' when dir = E -> { car with dir = N }
      | _ -> car
    in
    { car with pos = Pos.move_in_square pos car.dir }

  let move_pop q tbl cnt =
    let res = Queue.pop q in
    Hashtbl.remove tbl res.pos;
    move res cnt

  let main cnt =
    let q, tbl = parse_input cnt in
    let rec aux ({ pos; _ } as car) =
      match Hashtbl.find_opt tbl pos with
      | None ->
          Queue.add car q;
          Hashtbl.add tbl pos car;
          aux (move_pop q tbl cnt)
      | _ -> Printf.sprintf "%d,%d" pos.x pos.y
    in
    aux (move_pop q tbl cnt)
end

module P2 = struct
  include P1

  let filter_queue q car =
    let fst = Queue.pop q in
    Queue.add fst q;
    while Queue.peek q <> fst do
      let x = Queue.pop q in
      if x <> car then Queue.add x q
    done;
    if Queue.top q = car then Queue.take q |> ignore

  let main cnt =
    let q, tbl = parse_input cnt in
    let rec aux ({ pos; _ } as car) =
      match Queue.length q with
      | 0 -> Printf.sprintf "%d,%d" pos.x pos.y
      | _ ->
          (match Hashtbl.find_opt tbl pos with
          | None ->
              Hashtbl.add tbl pos car;
              Queue.add car q
          | Some old ->
              Hashtbl.remove tbl pos;
              filter_queue q old);
          aux (move_pop q tbl cnt)
    in
    aux (move_pop q tbl cnt)
end

let part1 () = P1.main (cnt ())
let part2 () = P2.main (cnt ())
