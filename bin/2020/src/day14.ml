open Lib
module Map14 = Map.Make (Int)

let add_padding n str = String.make n '0' ^ str

let parse_mask s = List.(String.split_on_char ' ' s |> tl |> tl |> hd)

let parse_mem m =
  let l = String.split_on_char ' ' m in
  let ad =
    let prov = List.(hd l |> String.split_on_char '[' |> tl |> hd) in
    String.sub prov 0 (String.length prov - 1)
  in
  (int_of_string ad, List.(l |> tl |> tl |> hd |> int_of_string |> int_to_bin))

let inp () =
  let l = read_file "day14" (fun i -> i) in
  let rec aux mask memL = function
    | [] -> [ (mask, memL |> List.rev) ]
    | hd :: tl when hd.[1] = 'e' -> aux mask (parse_mem hd :: memL) tl
    | hd :: tl -> (mask, memL |> List.rev) :: aux (parse_mask hd) [] tl
  in
  aux (parse_mask (List.hd l)) [] (List.tl l)

let calc_one (mask : string) value =
  let value' = add_padding (String.length mask - String.length value) value in
  List.fold_left2
    (fun acc e1 e2 ->
      acc ^ match e1 with 'X' -> char_of_string e2 | n -> char_of_string n)
    ""
    (mask |> string_2_char_list)
    (value' |> string_2_char_list)

let calc_all l =
  List.map
    (fun (mask, l') ->
      List.map (fun (mem, value) -> (mem, calc_one mask value |> bin_to_int)) l')
    l
  |> List.flatten
  |> List.fold_left (fun acc (ad, value) -> Map14.add ad value acc) Map14.empty

let day14_1 () = Map14.fold (fun _ a acc -> acc + a) (inp () |> calc_all) 0

module Map14' = Map.Make (String)

let create_all_comb =
  let rec aux acc = function
    | [] ->
        [
          List.fold_left (fun x elt -> char_of_string elt ^ x) "" acc
          |> bin_to_int;
        ]
    | hd :: tl when hd = 'X' -> aux acc ('1' :: tl) @ aux acc ('0' :: tl)
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux []

let calc_one' mask adress =
  let adress' =
    add_padding (String.length mask - String.length adress) adress
  in
  List.map2
    (fun e1 e2 -> match e1 with '0' -> e2 | n -> n)
    (mask |> string_2_char_list)
    (adress' |> string_2_char_list)
  |> create_all_comb

let calc_all' l =
  List.map
    (fun (mask, l') ->
      List.map
        (fun (mem, value) ->
          (calc_one' mask (int_to_bin mem), value |> bin_to_int))
        l')
    l
  |> List.flatten
  |> List.fold_left
       (fun acc (ads, value) ->
         List.fold_left (fun acc ad -> Map14.add ad value acc) acc ads)
       Map14.empty

let day14_2 () = Map14.fold (fun _ a acc -> acc + a) (inp () |> calc_all') 0
