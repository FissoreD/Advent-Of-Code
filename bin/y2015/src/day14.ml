let cnt =
  let l = Lib.read_file "15" "14" (String.split_on_char ' ') in
  let open List in
  map
    (fun l ->
      ( nth l 3 |> int_of_string,
        nth l 6 |> int_of_string,
        nth l 13 |> int_of_string ))
    l

module P1 = struct
  (** Returns (pos, dist) list *)
  let counter cnt total_time =
    List.mapi
      (fun pos (a, b, c) ->
        (pos, a * ((total_time / (b + c) * b) + min (total_time mod (b + c)) b)))
      cnt

  let main cnt = counter cnt 2503 |> List.map snd |> List.fold_left max 0
end

module P2 = struct
  let rec maxim_filter (maxim, l) = function
    | [] -> l
    | (pos, dist) :: tl when dist = maxim -> maxim_filter (maxim, pos :: l) tl
    | (_, dist) :: tl when dist < maxim -> maxim_filter (maxim, l) tl
    | (pos, dist) :: tl -> maxim_filter (dist, [ pos ]) tl

  let main cnt =
    let arr = Array.init (List.length cnt) (fun _ -> ref 0) in
    let rec aux = function
      | 0 -> ()
      | n ->
          let res = maxim_filter (0, []) (P1.counter cnt n) in
          List.iter (fun i -> incr arr.(i)) res;
          aux (n - 1)
    in
    aux 2503;
    Array.fold_left (fun acc value -> max acc !value) 0 arr
end

let part1 () = P1.main cnt |> print_int
let part2 () = P2.main cnt |> print_int
