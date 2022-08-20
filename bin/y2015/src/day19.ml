(* https://adventofcode.com/2015/day/19 *)

module MySet = Set.Make (struct
  type t = char list

  let compare = compare
end)

let cnt =
  let cnt = Lib.read_file "15" "19" Lib.id |> List.rev in
  let str = List.hd cnt |> Lib.string_2_char_list in
  let map =
    List.tl cnt |> List.tl
    |> List.map Re.Str.(split_delim (regexp " => "))
    |> List.map (function
         | [ h1; h2 ] ->
             (h1 |> Lib.string_2_char_list, h2 |> Lib.string_2_char_list)
         | _ -> invalid_arg "Parsing problem")
  in
  (str, map)

module P1 = struct
  let rec same_prefix : char list * char list -> bool * char list = function
    | [], [] -> (true, [])
    | [ h1 ], h2 :: tl | h1 :: tl, [ h2 ] -> (h1 = h2, tl)
    | h1 :: t1, h2 :: t2 when h1 = h2 -> same_prefix (t1, t2)
    | _ -> (false, [])

  let rec replaceOne (pred : char list) (key, value) : char list -> char list =
    function
    | [] -> []
    | hd :: tl as l ->
        if List.length l < List.length key then pred @ l
        else
          let same, suff = same_prefix (key, l) in
          if same then
            (* print_endline *)
            (* (Lib.char_list_2_string value ^ " " ^ Lib.char_list_2_string key); *)
            pred @ value @ suff
          else replaceOne (pred @ [ hd ]) (key, value) tl

  let replace (s : char list) ((a, b) : char list * char list) =
    let rec aux acc (pred : char list) = function
      | [] -> acc
      | hd :: tl as l ->
          if List.length l < List.length a then acc
          else
            let same, suff = same_prefix (a, l) in
            aux
              (if same then (pred @ b @ suff) :: acc else acc)
              (pred @ [ hd ]) tl
    in
    aux [] [] s

  let main ((s : char list), (l : (char list * char list) list)) =
    List.map (replace s) l |> List.flatten |> MySet.of_list |> MySet.cardinal
end

module P2 = struct
  let main ((s : char list), (assoc : (char list * char list) list)) =
    let assoc =
      List.map (fun (a, b) -> (b, a)) assoc
      |> List.sort (fun (a, _) (b, _) -> List.length b - List.length a)
    in
    let rec aux assoc1 acc values =
      let next = P1.replaceOne [] (List.hd assoc1) values in
      (* if next <> values then *)
      print_endline (string_of_int acc ^ " " ^ Lib.char_list_2_string values);
      if [ 'e' ] = next then acc
      else if List.length next = 0 then invalid_arg "Error ?"
      else
        aux
          (if next = values then List.tl assoc1 else assoc)
          (acc + if next = values then 0 else 1)
          next
    in
    aux assoc 1 s
end

let part1 () = P1.main cnt |> print_int

let part2 () =
  (* P1.replaceOne [] ([ 'H'; 'T'; 'H' ], [ 'F' ]) [ 'H'; 'O'; 'H'; 'H'; 'O' ]
     |> Lib.char_list_2_string |> print_endline *)
  P2.main cnt |> print_int
