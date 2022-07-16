(* https://adventofcode.com/2015/day/12 *)

let cnt = Lib.read_file "15" "12" Lib.id |> List.hd

module P1 = struct
  let parse_line line =
    let rec aux pos =
      try
        let sub_str_pos =
          Re.Str.search_forward (Re.Str.regexp {|-?[0-9]+|}) line pos
        in
        let sub_str = Re.Str.matched_string line in
        int_of_string sub_str + aux (sub_str_pos + String.length sub_str)
      with Not_found -> 0
    in
    aux 0
end

module P2 = struct
  let remove_red cnt =
    let cnt_parsed = Yojson.Basic.from_string cnt in
    let rec aux (ct : Yojson.Basic.t) =
      match ct with
      | `List l -> l |> List.fold_left (fun acc e -> acc + aux e) 0
      | `Assoc k ->
          if List.exists (fun (_, b) -> b = `String "red") k then 0
          else List.fold_left (fun acc e -> acc + aux e) 0 (List.map snd k)
      | `String _ -> 0
      | `Int i -> i
      | _ -> raise Lib.Invalid_input
    in
    aux cnt_parsed
end

let part1 () = P1.parse_line cnt |> print_int
let part2 () = P2.remove_red cnt |> print_int
