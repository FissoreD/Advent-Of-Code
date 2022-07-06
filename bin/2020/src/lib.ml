exception Invalid_input

module List = struct
  include List

  let rec pop elt = function
    | [] -> []
    | hd :: tl when hd = elt -> pop elt tl
    | _ :: tl -> pop elt tl

  let rec count elt = function
    | [] -> 0
    | hd :: tl when elt = hd -> 1 + count elt tl
    | _ :: tl -> count elt tl

  let get a b = List.nth b a

  let rec set index elt = function
    | [] -> []
    | hd :: tl when index = 0 -> elt :: tl
    | hd :: tl -> hd :: set (index - 1) elt tl

  let split_on_index i =
    let rec aux acc i = function
      | [] -> (acc, [])
      | hd :: tl when i = 0 -> (List.rev (hd :: acc), tl)
      | hd :: tl -> aux (hd :: acc) (i - 1) tl
    in
    aux [] (i - 1)

  let print f l = iter (fun elt -> f elt |> Printf.printf "%s ") l

  let transpose_matrix mat =
    init
      (hd mat |> length)
      (fun i -> init (length mat) (fun j -> get j mat |> get i))

  let rec sub start len = function
    | _ when len = 0 -> []
    | [] -> []
    | _ :: tl when start > 0 -> sub (start - 1) len tl
    | hd :: tl -> hd :: sub start (len - 1) tl

  let rec index_of elt = function
    | [] -> raise Invalid_input
    | hd :: _ when hd = elt -> 0
    | _ :: tl -> 1 + index_of elt tl
end

(** is int *)
let is_int s =
  try
    int_of_string s |> ignore;
    true
  with _ -> false

let string_2_char_list s =
  let rec aux acc = function
    | -1 -> acc
    | n -> aux (String.get s n :: acc) (n - 1)
  in
  aux [] (String.length s - 1)

let char_of_string = String.make 1

let rec char_list_2_string = function
  | [] -> ""
  | hd :: tl -> char_of_string hd ^ char_list_2_string tl

let openF s = open_in ("../input/" ^ s ^ ".txt")

let closeF = close_in

let read_file s f =
  let inp = openF s in
  let rec aux () =
    try
      let l = input_line inp in
      f l :: aux ()
    with End_of_file ->
      closeF inp;
      []
  in
  aux ()

let in_bouond_inclusive (a, b) elt = elt >= a && elt <= b

let xor a b = (a && not b) || (b && not a)

let bool_to_int n = if n then 1 else 0

let ( ++ ) (a, b) (c, d) = (a + c, b + d)

let ( ** ) (a, b) (c, d) = (a * c, b * d)

let ( +++ ) (a, b, c) (a', b', c') = (a + a', b + b', c + c')

let ( ++++ ) (a, b, c, d) (a', b', c', d') = (a + a', b + b', c + c', d + d')

let remove_empty_line l =
  let rec aux acc = function
    | [] -> [ acc ]
    | "" :: tl -> acc :: aux "" tl
    | hd :: tl -> aux (acc ^ (if acc = "" then acc else " ") ^ hd) tl
  in
  aux "" l

let ang_in_360 a = a - (a / 360 * 360)

let ang_compl a = 360 - ang_in_360 a

let int_to_bin = function
  | 0 -> "0"
  | n ->
      let rec aux = function
        | 0 -> ""
        | n -> aux (n / 2) ^ (n mod 2 |> string_of_int)
      in
      aux n

let bin_to_int s = "0b" ^ s |> int_of_string

let id i = i
