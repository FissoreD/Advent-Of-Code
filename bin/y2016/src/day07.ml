(* https://adventofcode.com/2016/day/7 *)

open String

let cnt () =
  Lib.read_file "16" "07" (Re.Str.split (Re.Str.regexp {|\(\[\|\]\)|}))

module P1 = struct
  let rec has_abba_word ?(pos = 0) s =
    if pos + 4 > length s then false
    else if
      get s pos = get s (pos + 3)
      && get s (pos + 1) = get s (pos + 2)
      && get s pos <> get s (pos + 1)
    then true
    else has_abba_word ~pos:(pos + 1) s

  let rec is_valid_line ?(is_valid = false) = function
    | [] -> is_valid
    | h1 :: h2 :: tl ->
        if has_abba_word h2 then false
        else is_valid_line ~is_valid:(has_abba_word h1 || is_valid) tl
    | [ hd ] -> is_valid || has_abba_word hd

  let main cnt = List.filter is_valid_line cnt |> List.length
end

module P2 = struct
  let rec aba_words ?(pos = 0) s =
    if pos + 3 > length s then []
    else if get s pos = get s (pos + 2) && get s pos <> get s (pos + 1) then
      sub s pos 3 :: aba_words ~pos:(pos + 1) s
    else aba_words ~pos:(pos + 1) s

  let rec has_corresponding_bab bab_list aba =
    match bab_list with
    | [] -> false
    | hd :: _ when get hd 0 = get aba 1 && get hd 1 = get aba 0 -> true
    | _ :: tl -> has_corresponding_bab tl aba

  let is_not_empty aba = List.exists (fun e -> e <> []) aba

  let rec is_valid_line ?(aba : t list = []) ?(bab = []) = function
    | [] -> List.length aba > 0 && List.exists (has_corresponding_bab bab) aba
    | h1 :: h2 :: tl ->
        is_valid_line ~aba:(aba_words h1 @ aba) ~bab:(aba_words h2 @ bab) tl
    | [ hd ] -> is_valid_line ~aba:(aba_words hd @ aba) ~bab []

  let main cnt = List.filter is_valid_line cnt |> List.length
end

let part1 () = P1.main (cnt ()) |> string_of_int |> print_endline
let part2 () = P2.main (cnt ()) |> string_of_int |> print_endline
