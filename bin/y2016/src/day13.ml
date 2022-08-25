(* https://adventofcode.com/2016/day/13 *)

type coor = { x : int; y : int }

let cnt = Lib.read_file "16" "13" int_of_string |> List.hd

module P1 = struct
  let start_pos = { x = 1; y = 1 }
  let is_valid { x; y } = x >= 0 && y >= 0

  let is_open { x; y } cnt =
    let n = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + cnt in
    Lib.count_substring (Lib.int_to_bin n) "1" mod 2 = 0

  let next_pos_list { x; y } cnt =
    [ { x = x + 1; y }; { x = x - 1; y }; { x; y = y + 1 }; { x; y = y - 1 } ]
    |> List.filter (fun pos -> is_valid pos && is_open pos cnt)

  let main ?(dest = { x = 31; y = 39 }) () =
    let explored, queue = (ref [], Queue.create ()) in
    let current_pos = ref (0, start_pos) in
    Queue.push (0, start_pos) queue;
    while snd !current_pos <> dest do
      current_pos := Queue.pop queue;
      if List.mem (snd !current_pos) !explored |> not then (
        explored := snd !current_pos :: !explored;
        let next = next_pos_list (snd !current_pos) cnt in
        List.iter (fun e -> Queue.push (fst !current_pos + 1, e) queue) next)
    done;
    fst !current_pos
end

module P2 = struct
  let main ?(max_dist = 50) () =
    let explored, queue, res = (ref [], Queue.create (), ref 0) in
    let current_pos = ref (0, P1.start_pos) in
    Queue.push (0, P1.start_pos) queue;
    while not (Queue.is_empty queue) do
      current_pos := Queue.pop queue;
      if
        (not (List.mem (snd !current_pos) !explored))
        && fst !current_pos <= max_dist
      then (
        incr res;
        explored := snd !current_pos :: !explored;
        let next = P1.next_pos_list (snd !current_pos) cnt in
        List.iter (fun e -> Queue.push (fst !current_pos + 1, e) queue) next)
    done;
    !res
end

let part1 () = P1.main () |> Printf.printf "%d\n"
let part2 () = P2.main ~max_dist:50 () |> Printf.printf "%d\n"
