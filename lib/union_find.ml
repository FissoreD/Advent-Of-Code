(** https://www.geeksforgeeks.org/union-find/ *)
let create n = Array.init n Fun.id

let find_parent arr n =
  let res = ref n in
  while arr.(!res) <> !res do
    arr.(!res) <- arr.(arr.(!res));
    res := arr.(!res)
  done;
  !res

let union arr a b =
  let p1 = find_parent arr a in
  let p2 = find_parent arr b in
  arr.(p1) <- p2

let same arr a b = find_parent arr a = find_parent arr b

let print a =
  Array.iter (Printf.printf "%d ") a;
  print_newline ()