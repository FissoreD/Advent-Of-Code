(* https://adventofcode.com/2018/day/7 *)

let cnt () =
  Lib.read_file "18" "07" (fun l ->
      let l = String.split_on_char ' ' l in
      List.(nth l 1, nth l 7))

module P1 = struct
  open Hashtbl
  module S = Set.Make (String)

  let find tbl k = Option.value ~default:[] (find_opt tbl k)

  let set2sort s1 s2 =
    S.diff s1 s2 |> S.to_seq |> List.of_seq |> List.sort compare

  let make_tbl cnt =
    let pointer, pointed = (ref S.empty, ref S.empty) in
    let before, after = (create 2048, create 2048) in
    List.iter
      (fun (a, b) ->
        pointer := S.add a !pointer;
        pointed := S.add b !pointed;
        replace before a (List.merge compare [ b ] (find before a));
        replace after b (List.merge compare [ a ] (find after b)))
      cnt;
    (before, after, set2sort !pointer !pointed)

  let main cnt =
    let before, after, start = make_tbl cnt in
    let current = ref start in
    let res = ref [] in
    while length before > 0 do
      let c = List.hd !current in
      let next =
        List.filter
          (fun e ->
            let pred = find after e |> Lib.List.remove c in
            replace after e pred;
            pred = [])
          (find before c)
      in
      remove before c;
      current := List.merge compare (List.tl !current) next;
      res := c :: !res
    done;
    List.rev_append !res !current |> String.concat ""
end

module P2 = struct
  open Hashtbl
  include P1

  let to_weight s = Char.code s.[0] - Char.code 'A' + 1 + 60
  let find_int tbl worker = Option.value ~default:0 (find_opt tbl worker)
  let add_task time tbl worker task = tbl.(worker) <- time + to_weight task

  let free_worker arr time =
    let len = Array.length arr in
    let rec aux = function
      | n when n = len -> None
      | n when arr.(n) <= time -> Some n
      | n -> aux (n + 1)
    in
    aux 0

  let main ~worker_nb cnt =
    let before, after, start = make_tbl cnt in
    let time, workers = (ref 0, Array.make worker_nb 0) in
    let todo, in_progress = (ref start, ref []) in
    while !todo <> [] || !in_progress <> [] do
      let finished, not_finished =
        List.partition (fun (_, end_t) -> end_t <= !time) !in_progress
      in
      in_progress := not_finished;
      List.iter
        (fun (task, _) ->
          (List.iter (fun next_task ->
               let pred = find after next_task in
               replace after next_task (List.filter (( <> ) task) pred);
               if List.length pred = 1 then
                 todo := List.merge compare [ next_task ] !todo))
            (find before task))
        finished;
      in_progress := List.filter (fun (_, end_t) -> end_t > !time) !in_progress;
      (try
         while !todo <> [] do
           match free_worker workers !time with
           | None -> raise Exit
           | Some worker ->
               let task = List.hd !todo in
               add_task !time workers worker task;
               in_progress := (task, !time + to_weight task) :: !in_progress;
               todo := List.tl !todo
         done
       with Exit -> ());
      incr time
    done;
    !time - 1
end

let part1 () = P1.main (cnt ())
let part2 () = P2.main ~worker_nb:5 (cnt ()) |> string_of_int
