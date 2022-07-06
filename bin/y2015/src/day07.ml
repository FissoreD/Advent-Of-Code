let cnt = Lib.read_file "15" "07" (String.split_on_char ' ')

module MyMap = Map.Make (String)

module P1 = struct
  let parse_symbol map a =
    if Lib.is_int a then a |> int_of_string else MyMap.find a map

  let func_to_op dico value st =
    (match st with
    | "AND" -> ( land )
    | "OR" -> ( lor )
    | "LSHIFT" -> fun b a -> a lsl b
    | "RSHIFT" -> fun b a -> a lsr b
    | _ -> raise Lib.Invalid_input)
      (parse_symbol dico value)

  let val_to_some a _ = Some a
  let updateDico key value map = MyMap.update key (val_to_some value) map

  let parse_rule map = function
    | [ a; "->"; b ] -> updateDico b (parse_symbol map a) map
    | [ x; op; y; "->"; z ] ->
        updateDico z
          (min (max 0 (parse_symbol map x |> func_to_op map y op)) 65535)
          map
    | [ "NOT"; x; "->"; y ] ->
        updateDico y (65535 land lnot (parse_symbol map x)) map
    | _ -> raise Lib.Invalid_input

  let is_valid map value =
    try
      match value with
      | [ x; _; y; "->"; _ ] ->
          parse_symbol map x + parse_symbol map y |> ignore;
          true
      | [ x; "->"; _ ] | [ "NOT"; x; "->"; _ ] ->
          parse_symbol map x |> ignore;
          true
      | _ -> false
    with Not_found -> false

  let main rules =
    let rec aux map = function
      | [] -> map
      | rules ->
          let filter, others = List.partition (fun a -> is_valid map a) rules in
          aux (List.fold_left parse_rule map filter) others
    in
    aux MyMap.empty rules
end

let part1 () = P1.main cnt |> MyMap.find "a" |> print_int

let part2 () =
  let res = P1.main cnt |> MyMap.find "a" in
  let new_cnt =
    let rec aux acc = function
      | [] -> acc
      | hd :: tl ->
          if Lib.List.get (List.length hd - 1) hd = "b" then
            aux ([ res |> string_of_int; "->"; "b" ] :: acc) tl
          else aux (hd :: acc) tl
    in
    aux [] cnt
  in
  P1.main new_cnt |> MyMap.find "a" |> print_int
