(* https://adventofcode.com/2016/day/9 *)

open Re.Str

let cnt = Lib.read_file "16" "09" Lib.id

module P1 = struct
  let regex = regexp {|(\([0-9]*\)x\([0-9]*\))|}
  let next_marker = search_forward regex

  let main () =
    let rec aux pos cnt =
      try
        let pos1 = next_marker cnt pos in
        let len, rep = (matched_group 1 cnt, matched_group 2 cnt) in
        pos1 - pos
        + (int_of_string len * int_of_string rep)
        + aux (pos1 + 3 + String.length (len ^ rep) + int_of_string len) cnt
      with Not_found -> String.length cnt - pos
    in
    List.map (aux 0) cnt |> List.fold_left ( + ) 0
end

module P2 = struct
  let mainkk () =
    let rec aux cnt =
      try
        let pos1 = P1.next_marker cnt 0 in
        let len, rep = (matched_group 1 cnt, matched_group 2 cnt) in
        let substring =
          String.sub cnt
            (pos1 + String.length (len ^ rep) + 3)
            (int_of_string len)
        in
        let new_cnt =
          replace_first P1.regex
            (Lib.repeat_string (int_of_string rep - 1) substring)
            cnt
        in
        aux new_cnt
      with Not_found -> String.length cnt
    in
    List.map aux cnt |> List.fold_left ( + ) 0

  let main () =
    let rec aux (s : int) (e : int) (cnt : string) =
      try
        let pos1 = P1.next_marker cnt s in
        if pos1 >= e then e - s
        else
          let len = int_of_string @@ matched_group 1 cnt in
          let rep = int_of_string @@ matched_group 2 cnt in
          let string_matched_length = matched_string cnt |> String.length in
          let new_s = pos1 + string_matched_length + len in
          (rep * aux (new_s - len) (min e new_s) cnt)
          + pos1 - s + aux new_s e cnt
      with Not_found -> e - s
    in
    List.map (fun e -> aux 0 (String.length e) e) cnt |> List.fold_left ( + ) 0
end

let part1 () = P1.main () |> string_of_int |> print_endline
let part2 () = P2.main () |> string_of_int |> print_endline