open Lib
module Set07 = Set.Make (String)

let rec read_queue = function
  | nb :: col :: col2 :: tl when is_int nb ->
      (col ^ " " ^ col2, int_of_string nb) :: read_queue tl
  | hd :: tl when Str.string_match (Str.regexp "bag.*") hd 0 -> read_queue tl
  | _ -> []

let parse_line l =
  let open List in
  let first = String.split_on_char ' ' l in
  let name = hd first ^ " " ^ hd (tl first) in
  let queue = tl (tl (tl (tl first))) in
  (name, read_queue queue)

let inp () = read_file "day07" parse_line

let rec find_parent l (bag : string) =
  match l with
  | [] -> []
  | ((b : string), l) :: tl when List.mem_assoc bag l ->
      (b, List.assoc bag l) :: find_parent tl bag
  | _ :: tl -> find_parent tl bag

let rec find_all_parent (bag : string) l =
  let acc = ref Set07.empty in
  let rec aux current =
    let parents = find_parent l current in
    List.iter
      (fun (elt, _) ->
        acc := Set07.add elt !acc;
        aux elt)
      parents
  in
  aux bag;
  Set07.cardinal !acc

let day07_1 () = inp () |> find_all_parent "shiny gold"

let rec calc_all (name, children) (l : (string * (string * int) list) list) =
  List.fold_left
    (fun acc (name, number) ->
      acc + (number * calc_all (name, List.assoc name l) l))
    1 children

let day07_2 () =
  let inp' = inp () in
  calc_all ("shiny gold", List.assoc "shiny gold" inp') inp' - 1
