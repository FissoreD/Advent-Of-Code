(* https://adventofcode.com/2018/day/12 *)

type conf = {
  mutable min : int;
  mutable max : int;
  str : (int, char) Hashtbl.t;
  rules : (string, char) Hashtbl.t;
}

let cnt () =
  let rules = Hashtbl.create 32 in
  let cnt = Lib.read_file "18" "12" Fun.id in
  let init_config = List.(nth (hd cnt |> String.split_on_char ' ') 2) in
  let str = Hashtbl.create 2048 in
  String.iteri (fun p i -> Hashtbl.add str p i) init_config;
  let rec aux = function
    | [] -> { str; rules; min = 0; max = Hashtbl.length str - 1 }
    | hd :: tl ->
        (match Re.Str.(split (regexp " => ") hd) with
        | a :: [ b ] -> Hashtbl.add rules a b.[0]
        | _ -> invalid_arg "Y18D12 Invalid input");
        aux tl
  in
  aux List.(tl (tl cnt))

module P1 = struct
  let find tbl k = Option.value ~default:'.' (Hashtbl.find_opt tbl k)
  let make_str tbl pos = String.init 5 (fun i -> find tbl (pos - 5 + i + 3))

  let evolve (p : conf) =
    let clone = Hashtbl.copy p.str in
    let replace_rule k _ =
      let str = make_str clone k in
      if String.exists (( <> ) '.') str then (
        p.min <- min p.min k;
        p.max <- max p.max k;
        Hashtbl.replace p.str k (find p.rules str))
    in
    Hashtbl.iter replace_rule clone;
    for _ = 1 to 4 do
      replace_rule (p.min - 1) ();
      replace_rule (p.max + 1) ()
    done

  let get_sum cnt =
    Hashtbl.fold (fun k v acc -> acc + if v = '#' then k else 0) cnt.str 0

  let main ~times cnt =
    for _ = 1 to times do
      evolve cnt
    done;
    get_sum cnt
end

module P2 = struct
  let main ~times cnt =
    let rec aux s1 s2 n =
      P1.evolve cnt;
      let s3 = P1.get_sum cnt in
      if s3 - s2 = s2 - s1 then s1 + ((times - n + 1) * (s2 - s1))
      else aux s2 s3 (n + 1)
    in
    aux 0 0 0
end

let part1 () = P1.main ~times:20 (cnt ()) |> string_of_int
let part2 () = P2.main ~times:50_000_000_000 (cnt ()) |> string_of_int
