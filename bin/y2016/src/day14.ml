(* https://adventofcode.com/2016/day/14 *)

type hist_cell = { key : string; i : int; hash : string; five_list : char list }
type history = hist_cell Deque.t

let cnt () = Lib.read_file "16" "14" Fun.id |> List.hd

module P1 = struct
  let hash base i = Lib.md5_to_hex @@ Printf.sprintf "%s%d" base i

  let same5 s =
    let rec aux acc = function
      | a :: b :: c :: d :: e :: tl when a = b && b = c && c = d && d = e ->
          aux (a :: acc) tl
      | _ :: tl -> aux acc tl
      | _ -> acc
    in
    aux [] s

  let same3 s : char option =
    let rec aux = function
      | a :: b :: c :: _ when a = b && b = c -> Some a
      | _ :: tl -> aux tl
      | _ -> None
    in
    aux s

  let make_history_cell hash key i : hist_cell =
    let hash = hash key i in
    { key; i; hash; five_list = same5 (Lib.string_2_char_list hash) }

  let next_1000 hash base i : history =
    let q = Deque.create () in
    let rec aux = function
      | 0 -> q
      | n ->
          let same5 = make_history_cell hash base (1001 + i - n) in
          Deque.prepend same5 q;
          aux (n - 1)
    in
    aux 1000

  let is_a_key h (history : history) =
    let same3 = same3 (Lib.string_2_char_list h) in
    same3 <> None
    && Deque.exists
         (fun { five_list; _ } ->
           List.exists (fun e -> same3 = Some e) five_list)
         history

  let main ?(alt = 64) ?(hash = hash) cnt =
    let base = cnt in
    let q = next_1000 hash base 0 in
    let rec aux alt i =
      if alt = 0 then i - 1
      else
        let h = hash base i in
        let is_a_key = is_a_key h q in
        Deque.delete_last q |> ignore;
        Deque.prepend (make_history_cell hash base (1001 + i)) q;
        aux (alt - Lib.bool_to_int is_a_key) (i + 1)
    in
    aux alt 0
end

module P2 = struct
  let hash base i =
    let hash = P1.hash base i in
    let rec aux x = function 0 -> x | n -> aux (Lib.md5_to_hex x) (n - 1) in
    aux hash 2016

  let main = P1.main ~hash
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
