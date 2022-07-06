open Lib

let inp () =
  read_file "day08" (fun elt ->
      let l = String.split_on_char ' ' elt in
      (List.hd l, List.hd (List.tl l) |> int_of_string))

let make_step start acc l =
  let rec aux pos indexes res =
    if List.mem pos indexes then (res, pos)
    else
      try
        match List.nth l pos with
        | "nop", _ -> aux (pos + 1) (pos :: indexes) res
        | "acc", n -> aux (pos + 1) (pos :: indexes) (res + n)
        | _, n -> aux (pos + n) (pos :: indexes) res
      with Failure tl -> (res, pos)
  in
  aux start [] acc

let day08_1 () = inp () |> make_step 0 0 |> fst

let make_step' l =
  let len = List.length l in
  let rec aux (res, pos) =
    if pos = len then res
    else
      match List.nth l pos with
      | "nop", n when n <> 0 ->
          let res', pos' = make_step pos res (List.set pos ("jmp", n) l) in
          if pos' = len then res' else aux (res, pos + 1)
      | "nop", _ -> aux (res, pos + 1)
      | "jmp", n ->
          let res', pos' = make_step pos res (List.set pos ("nop", n) l) in
          if pos' = len then res' else aux (res, pos + n)
      | _, n -> aux (res + n, pos + 1)
  in
  aux (0, 0)

let day08_2 () = inp () |> make_step'
