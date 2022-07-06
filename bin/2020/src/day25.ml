open Lib

let subject_number = 7

let inp () = read_file "day25" int_of_string

let handshake x =
  let rec aux acc = function
    | e when e = x && acc != 0 -> acc
    | e -> aux (acc + 1) (e * subject_number mod 20201227)
  in
  aux 0 1

let encryption_key subject_number key =
  let rec aux key = function
    | 0 -> key
    | n -> aux (key * subject_number mod 20201227) (n - 1)
  in
  aux 1 key

let day25_1 () =
  let i = inp () in
  let a = List.hd i in
  let b = List.hd (List.tl i) in
  let ka = handshake a in
  let kb = handshake b in
  let res = encryption_key a kb in
  assert (res = encryption_key b ka);
  res
