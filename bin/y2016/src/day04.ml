(* https://adventofcode.com/2016/day/4 *)

type room = { name : string list; id : int; check_sum : string }

let cnt : room list =
  let funct (s : string) : room =
    let s = String.split_on_char '-' s |> List.rev in
    let hd, name = (List.hd s, List.rev (List.tl s)) in
    let id = int_of_string (List.hd (String.split_on_char '[' hd)) in
    let check_sum = String.sub hd (String.length hd - 6) 5 in
    { name; id; check_sum }
  in
  Lib.read_file "16" "04" funct |> List.rev

module P1 = struct
  let pos_to_char i = Char.chr (Char.code 'a' + i)
  let char_to_pos letter = Char.code letter - Char.code 'a'

  let real_rooms cnt =
    List.filter
      (fun { name; check_sum; _ } ->
        let name = List.fold_left ( ^ ) "" name |> Lib.string_2_char_list in
        let l = Array.init 26 (fun i -> (pos_to_char i, ref 0)) in
        List.iter (fun letter -> incr (l.(char_to_pos letter) |> snd)) name;
        Array.sort
          (fun (c, a) (d, b) ->
            let res = compare !b !a in
            if res = 0 then compare c d else res)
          l;
        try
          for i = 0 to 4 do
            if String.get check_sum i <> fst l.(i) then raise Exit
          done;
          true
        with Exit -> false)
      cnt

  let main () =
    real_rooms cnt |> List.map (fun { id; _ } -> id) |> List.fold_left ( + ) 0
end

module P2 = struct
  let next_letter letter = P1.pos_to_char @@ ((P1.char_to_pos letter + 1) mod 26)

  let decrypt { name; id; _ } =
    let rec aux name = function
      | 0 -> name
      | n -> aux (List.map (String.map next_letter) name) (n - 1)
    in
    aux name id

  let main () =
    (List.find (fun inp -> List.mem "northpole" (decrypt inp)) cnt).id
end

let part1 () = P1.main () |> string_of_int |> print_endline
let part2 () = P2.main () |> string_of_int |> print_endline
