(* https://adventofcode.com/2017/day/9 *)

let cnt () = Lib.read_file "17" "09" Fun.id |> List.hd

module P1 = struct
  let parse_line l =
    let open Re.Str in
    global_replace (regexp "!.") "" l
    |> global_replace (regexp "<[^>]*>") ""
    |> global_replace (regexp ",") ""
    |> Lib.string_2_char_list

  let main l =
    let rec aux acc cnt = function
      | [] -> acc
      | '}' :: tl -> aux acc (cnt - 1) tl
      | '{' :: tl -> aux (acc + cnt + 1) (cnt + 1) tl
      | _ -> failwith "error"
    in
    aux 0 0 (parse_line l)
end

module P2 = struct
  let main l =
    let open Re.Str in
    let l = global_replace (regexp "!.") "" l in
    let rec aux acc pos =
      try
        let ind = search_forward (regexp "<[^>]*>") l pos in
        let matched = matched_group 0 l in
        let len = String.length matched in
        print_endline matched;
        aux (acc + len - 2) (ind + len)
      with Not_found -> acc
    in
    aux 0 0
end

let part1 () = P1.main (cnt ()) |> Lib.print_int
let part2 () = P2.main (cnt ()) |> Lib.print_int
