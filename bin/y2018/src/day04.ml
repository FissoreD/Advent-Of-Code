(* https://adventofcode.com/2018/day/4 *)

let cnt () = Lib.read_file "18" "04" Fun.id |> List.sort compare

module P1 = struct
  type action = Asleep of int | Awake of int | Start of (int * int)

  type guard_stats = {
    mutable tot_sleep : int;
    mutable sleep_intervals : (int * int) list;
    id : int;
  }

  type min_sleep = { times : int; id : int }

  let empty_guard id = { tot_sleep = 0; sleep_intervals = []; id }

  let find tbl k =
    Option.value ~default:(empty_guard k) (Hashtbl.find_opt tbl k)

  let parse_line str =
    let str_split = String.split_on_char ' ' str in
    let ints = Lib.find_all_ints_in_string str in
    let hour = List.nth ints 3 in
    let min = if hour = 0 then List.nth ints 4 else 0 in
    match List.(hd (rev str_split)) with
    | "up" -> Awake min
    | "asleep" -> Asleep min
    | _ -> Start (min, List.nth ints 5)

  let find_most_asleep tbl =
    Hashtbl.fold
      (fun _k cnt acc -> if acc.tot_sleep < cnt.tot_sleep then cnt else acc)
      tbl (empty_guard (-1))

  let build_stats tbl =
    let rec aux cnt_guard start_sleep = function
      | [] -> ()
      | hd :: tl -> (
          match parse_line hd with
          | Awake min ->
              let guard = find tbl cnt_guard in
              guard.tot_sleep <- guard.tot_sleep + min - start_sleep;
              guard.sleep_intervals <-
                (start_sleep, min - 1) :: guard.sleep_intervals;
              Hashtbl.replace tbl cnt_guard guard;
              aux cnt_guard min tl
          | Asleep min -> aux cnt_guard min tl
          | Start (min, id) -> aux id min tl)
    in
    aux 0 0

  let find_most_minute_asleep { sleep_intervals; _ } : min_sleep =
    let tbl = Hashtbl.create 60 in
    let find k = Option.value ~default:0 (Hashtbl.find_opt tbl k) in
    let incr k = Hashtbl.replace tbl k (1 + find k) in
    List.iter
      (fun (a, b) ->
        for i = a to b do
          incr i
        done)
      sleep_intervals;
    Hashtbl.fold
      (fun k v acc ->
        if find acc.id < v then { id = k; times = find k } else acc)
      tbl { id = 0; times = 0 }

  let main cnt =
    let tbl = Hashtbl.create 2048 in
    build_stats tbl cnt;
    let most_sleep = find_most_asleep tbl in
    most_sleep.id * (find_most_minute_asleep most_sleep).id
end

module P2 = struct
  include P1

  let main cnt =
    let tbl = Hashtbl.create 2048 in
    build_stats tbl cnt;
    let guard, { id; _ } =
      Hashtbl.fold
        (fun _ k (g, old) ->
          let stat = find_most_minute_asleep k in
          if stat.times > old.times then (k, stat) else (g, old))
        tbl
        (empty_guard 0, { id = 0; times = 0 })
    in
    guard.id * id
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
