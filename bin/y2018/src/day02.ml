(* https://adventofcode.com/2018/day/2 *)

let cnt () = Lib.read_file "18" "02" Lib.string_2_char_list

module P1 = struct
  let rec with2same = function
    | [] -> false
    | a :: [ b ] when a = b -> true
    | a :: b :: c :: _ when a = b && b <> c -> true
    | a :: b :: c :: tl when a = b && b = c -> with2same tl
    | _ :: tl -> with2same tl

  let rec with3same = function
    | [] -> false
    | a :: b :: [ c ] when a = b && b = c -> true
    | a :: b :: c :: d :: _ when a = b && b = c && c <> d -> true
    | a :: b :: c :: d :: tl when a = b && b = c && c = d -> with3same tl
    | _ :: tl -> with3same tl

  let main cnt =
    let a, b =
      List.fold_left
        (fun (a, b) e ->
          let e = List.sort compare e in
          (a + Lib.bool_to_int (with2same e), b + Lib.bool_to_int (with3same e)))
        (0, 0) cnt
    in
    a * b
end

module P2 = struct
  let rec distance_one acc = function
    | [], [] -> acc
    | a :: l1, b :: l2 when a = b -> distance_one (a :: acc) (l1, l2)
    | _ :: l1, _ :: l2 when l1 = l2 -> List.rev_append acc l1
    | _ -> []

  let main cnt =
    let arr = Array.of_list cnt in
    let len = Array.length arr in
    let res = ref [] in
    (try
       for i = 0 to len - 1 do
         for j = i + 1 to len - 1 do
           let d = distance_one [] (arr.(i), arr.(j)) in
           if d <> [] then (
             res := d;
             raise Exit)
         done
       done
     with Exit -> ());
    List.map Lib.string_of_char !res |> String.concat ""
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ())
