(* https://adventofcode.com/2017/day/16 *)

let cnt () = Lib.read_file "17" "16" (String.split_on_char ',') |> List.hd

module P1 = struct
  let arr_len = 16

  let make_start_arr () =
    Array.init arr_len (fun i -> char_of_int (int_of_char 'a' + i))

  let rotate arr step =
    let len = Array.length arr in
    let step = step mod len in
    let start_pos = len - step in
    let store = Array.sub arr start_pos step in
    for i = 1 to len - step do
      arr.(len - i) <- arr.(len - i - step)
    done;
    Array.iteri (fun i e -> arr.(i) <- e) store

  let parse_rule arr r =
    let swap p1 p2 =
      let tmp = arr.(p1) in
      arr.(p1) <- arr.(p2);
      arr.(p2) <- tmp
    in
    let ints = Lib.find_all_ints_in_string r in
    match r.[0] with
    | 's' -> rotate arr (List.hd ints)
    | 'x' -> swap (List.hd ints) (List.nth ints 1)
    | 'p' ->
        let p1 = Lib.Array.findi (( = ) r.[1]) arr in
        let p2 = Lib.Array.findi (( = ) r.[3]) arr in
        swap p1 p2
    | _ -> failwith "Error"

  let run_dance cnt =
    let arr = make_start_arr () in
    List.iter (parse_rule arr) cnt;
    arr

  let arr_to_string x =
    Array.to_list x |> List.map Lib.string_of_char |> String.concat ""

  let main cnt = run_dance cnt |> arr_to_string
end

module P2 = struct
  open P1

  let rec find_loop cnt arr_start (it, mem) arr =
    List.iter (parse_rule arr) cnt;
    if arr = arr_start then (it + 1, mem)
    else find_loop cnt arr_start (it + 1, Array.map Fun.id arr :: mem) arr

  let main cnt =
    let mk = make_start_arr in
    let loop_size, mem = find_loop cnt (mk ()) (0, [ mk () ]) (mk ()) in
    List.nth mem (loop_size - 1 - (1_000_000_000 mod loop_size))
    |> arr_to_string
end

let part1 () = P1.main (cnt ())
let part2 () = P2.main (cnt ())
