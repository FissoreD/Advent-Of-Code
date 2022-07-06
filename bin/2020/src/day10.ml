open Lib

let inp () = read_file "day10" int_of_string |> List.sort compare

let calc l =
  let rec aux current = function
    | [] -> []
    | hd :: tl when hd < current + 4 -> hd :: aux hd tl
    | _ -> raise Invalid_input
  in
  aux 0 l

let rec diff_of_n (n : int) = function
  | [] | [ _ ] -> 1
  | hd :: hd' :: tl when hd' - hd = n -> 1 + diff_of_n n (hd' :: tl)
  | _ :: tl -> diff_of_n n tl

let day10_1 () =
  let l = inp () |> calc in
  diff_of_n 3 l * diff_of_n 1 l

let calc' l =
  let res = ref [ (List.fold_left max 0 l, 1) ] in
  let l' = List.rev (0 :: l) in
  let rec aux = function
    | [] -> List.assoc 0 !res
    | hd :: tl ->
        let l'' =
          List.filter
            (fun (elt, _) -> List.mem elt [ hd + 1; hd + 2; hd + 3 ])
            !res
        in
        res := (hd, List.fold_left (fun acc (_, ct) -> acc + ct) 0 l'') :: !res;
        aux tl
  in
  aux l'

let day10_2 () = inp () |> calc'
