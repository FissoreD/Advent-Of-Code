(* https://adventofcode.com/2015/day/1 *)

let cnt () = Lib.read_file "15" "01" Lib.string_2_char_list

module P1 = struct
  let count_parenthesis n =
    let rec aux counter = function
      | [] -> counter
      | '(' :: l -> aux (counter + 1) l
      | ')' :: l -> aux (counter - 1) l
      | _ -> raise Lib.Invalid_input
    in
    aux 0 n
end

module P2 = struct
  let count_first_down n =
    let rec aux pos counter l =
      if counter = -1 then pos
      else
        match l with
        | [] -> counter
        | '(' :: l -> aux (pos + 1) (counter + 1) l
        | ')' :: l -> aux (pos + 1) (counter - 1) l
        | _ -> raise Lib.Invalid_input
    in
    aux 0 0 n
end

let part1 () =
  match cnt () with
  | [ cnt ] -> cnt |> P1.count_parenthesis |> string_of_int
  | _ -> raise Lib.Invalid_input

let part2 () =
  match cnt () with
  | [ cnt ] -> cnt |> P2.count_first_down |> string_of_int
  | _ -> raise Lib.Invalid_input
