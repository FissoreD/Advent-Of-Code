open Lib

let s2cl = string_2_char_list

let ioc x = int_of_char x - int_of_char '0'

let inp () = List.(read_file "day23" s2cl |> hd |> map ioc)

type 'a t = { hd : 'a; picked : 'a list; tl : 'a list }

let new_l l =
  { hd = List.hd l; picked = List.sub 1 3 l; tl = List.sub 4 Int.max_int l }

let find_pos { hd; picked; tl } =
  let tl' = List.sort compare tl in
  let rec find_next_step = function
    | [] -> raise Invalid_input
    | [ n ] -> n
    | h :: h' :: _ when h < hd && hd < h' -> h
    | _ :: tl -> find_next_step tl
  in
  let elt = find_next_step tl' in
  let rec make_new_list = function
    | [] -> raise Invalid_input
    | h :: t when h = elt -> (h :: picked) @ t @ [ hd ]
    | h :: t -> h :: make_new_list t
  in
  make_new_list tl

let rec iterate l = function 0 -> l | n -> iterate (find_pos (new_l l)) (n - 1)

let rec create_res acc = function
  | [] -> raise Invalid_input
  | 1 :: tl -> tl @ List.rev acc
  | h :: tl -> create_res (h :: acc) tl

let int_list_2_string = List.fold_left (fun a e -> a ^ string_of_int e) ""

let day23_1 () = iterate (inp ()) 100 |> create_res [] |> int_list_2_string

module M = struct
  include Map.Make (Int)

  let print = iter (fun a b -> Printf.printf "(%d -> %d)" a b)
end

let maxInt = 1_000_000 - List.length (inp ())

let inp' () =
  let i = inp () in
  let l = List.length i in
  let i = i @ List.init maxInt (fun i -> i + l + 1) in
  let rec aux m = function
    | [] -> m
    | [ x ] -> M.add x (List.hd i) m
    | h :: h' :: t -> aux (M.add h h' m) (h' :: t)
  in
  aux M.empty i

let find_pos' (m, elt) =
  let a = M.find elt m in
  let b = M.find a m in
  let c = M.find b m in
  let d = M.find c m in
  let m = M.add elt d m in
  let l = [ a; b; c ] in
  let destin =
    let ex = List.exists in
    let rec f hyp =
      if ex (( = ) hyp) l |> not && hyp > 0 then hyp
      else
        let hyp = if hyp < 1 then M.cardinal m + 1 else hyp in
        f (hyp - 1)
    in
    f (elt - 1)
  in
  let m = M.add c (M.find destin m) m in
  let m = M.add destin a m in
  (m, d)

let rec count f arg = function 1 -> f arg | n -> count f (f arg) (n - 1)

let day23_2 n =
  let a, b = count find_pos' (inp' (), List.hd (inp ())) n in
  M.find 1 a * M.find (M.find 1 a) a

(* let _ = day23_2 10_000_000 |> print_int *)
