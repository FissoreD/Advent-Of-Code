(* https://adventofcode.com/2015/day/5 *)

let cnt = Lib.read_file "15" "05" Lib.id

module P1 = struct
  let is_fine s =
    let vowels = "aeiou" in
    let f a c = a + if String.contains vowels c then 1 else 0 in
    String.fold_left f 0 s > 2
    && Re.Str.string_match (Re.Str.regexp {|.*\(ab\|cd\|pq\|xy\).*|}) s 0 |> not
    && Re.Str.string_match
         (Re.Str.regexp
            {|.*\(aa\|bb\|cc\|dd\|ee\|ff\|gg\|hh\|ii\|jj\|kk\|ll\|mm\|nn\|oo\|pp\|qq\|rr\|ss\|tt\|uu\|vv\|ww\|xx\|yy\|zz\).*|})
         s 0

  let is_fine_fold x =
    List.map is_fine x |> List.map Lib.bool_to_int |> List.fold_left ( + ) 0
end

module P2 = struct
  let rec list_contains (a, b) = function
    | a1 :: b1 :: _ when a1 = a && b1 = b -> true
    | _ :: l -> list_contains (a, b) l
    | _ -> false

  let rec loop_on_list = function
    | a :: b :: l ->
        if list_contains (a, b) l then true else loop_on_list (b :: l)
    | _ -> false

  let is_fine s =
    let sList = Lib.string_2_char_list s in
    loop_on_list sList
    && Re.Str.string_match
         (Re.Str.regexp
            {|.*\(a.a\|b.b\|c.c\|d.d\|e.e\|f.f\|g.g\|h.h\|i.i\|j.j\|k.k\|l.l\|m.m\|n.n\|o.o\|p.p\|q.q\|r.r\|s.s\|t.t\|u.u\|v.v\|w.w\|x.x\|y.y\|z.z\).*|})
         s 0

  let is_fine_fold x =
    List.map is_fine x |> List.map Lib.bool_to_int |> List.fold_left ( + ) 0
end

let part1 () = P1.is_fine_fold cnt |> print_int
let part2 () = P2.is_fine_fold cnt |> print_int
