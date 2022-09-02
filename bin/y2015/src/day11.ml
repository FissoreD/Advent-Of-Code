(* https://adventofcode.com/2015/day/11 *)

let cnt () = Lib.read_file "15" "11" Lib.string_2_char_list |> List.hd

module P1 = struct
  let char_a = int_of_char 'a'
  let char_2_int (c : char) = int_of_char c - char_a
  let int_2_char (i : int) = char_of_int (i + char_a)

  let rec has_three_consecutive_letters = function
    | [] -> false
    | h1 :: h2 :: h3 :: _ when h1 = h2 + 1 && h2 = h3 + 1 -> true
    | _ :: tl -> has_three_consecutive_letters tl

  let rec has_invalid_letter = function
    | [] -> false
    | hd :: _ when int_2_char hd = 'i' -> true
    | hd :: _ when int_2_char hd = 'o' -> true
    | hd :: _ when int_2_char hd = 'l' -> true
    | _ :: tl -> has_invalid_letter tl

  let has_2_repetitions (l : int list) =
    let rec aux = function
      | 2, _ -> true
      | n, h1 :: h2 :: tl when h1 = h2 -> aux (n + 1, tl)
      | n, _ :: tl -> aux (n, tl)
      | _, [] -> false
    in
    aux (0, l)

  let rec next = function
    | hd :: tl when hd < 25 -> (hd + 1) :: tl
    | _ :: tl -> 0 :: next tl
    | [] -> [ 0 ]

  let print f =
    f |> List.rev |> List.iter (fun f -> print_char (int_2_char f));
    print_newline ()

  let rec find_next_word l =
    let x = next l in
    if
      has_2_repetitions x
      && (not (has_invalid_letter x))
      && has_three_consecutive_letters x
    then x
    else find_next_word x

  let main cnt = List.map char_2_int cnt |> List.rev |> find_next_word
end

module P2 = struct
  let main x = P1.main x |> P1.find_next_word
end

let part1 () = P1.main (cnt ()) |> P1.print
let part2 () = P2.main (cnt ()) |> P1.print
