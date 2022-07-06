open Lib

let inp () = read_file "day04" (fun f -> f)

let parse_line l =
  let l =
    String.split_on_char ' ' l
    |> List.map (fun i ->
           let l' = String.split_on_char ':' i in
           (List.hd l', List.hd (List.tl l')))
  in
  l |> List.remove_assoc "cid" |> List.sort (fun (a, _) (b, _) -> compare a b)

let valid_line l = List.length l = 7 |> bool_to_int

let day04_1 () =
  inp () |> remove_empty_line |> List.map parse_line |> List.map valid_line
  |> List.fold_left ( + ) 0

let valid_line2 l =
  if List.length l <> 7 then 0
  else
    try
      let aux x elt = in_bouond_inclusive x (int_of_string elt) in
      let conds =
        [
          ("byr", fun elt -> aux (1920, 2002) elt);
          ("iyr", fun elt -> aux (2010, 2020) elt);
          ("eyr", fun elt -> aux (2020, 2030) elt);
          ( "hgt",
            fun s ->
              let i = String.sub s 0 (String.length s - 2) in
              aux (if String.contains s 'c' then (150, 193) else (59, 76)) i );
          ( "hcl",
            fun elt ->
              elt.[0] = '#'
              && is_int ("0x" ^ String.sub elt 1 (String.length elt - 1)) );
          ( "ecl",
            fun elt ->
              [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
              |> List.exists (fun x -> x = elt) );
          ("pid", fun elt -> String.length elt = 9 && is_int elt);
        ]
        |> List.sort (fun (a, _) (b, _) -> compare a b)
      in
      let f (x, y) (x', y') =
        if x <> x' || not (y' y) then raise Invalid_input
      in
      List.iter2 f l conds;
      1
    with _ -> 0

let day04_2 () =
  inp () |> remove_empty_line |> List.map parse_line |> List.map valid_line2
  |> List.fold_left ( + ) 0
