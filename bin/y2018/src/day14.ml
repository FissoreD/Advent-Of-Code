(* https://adventofcode.com/2018/day/14 *)

let cnt () = Lib.read_file "18" "14" Fun.id |> List.hd

module P1 = struct
  let char_to_int =
    let zero = Char.code '0' in
    fun c -> Char.code c - zero

  let main ~part1 (cnt : string) =
    let open Buffer in
    let stop = if part1 then int_of_string cnt else 50_000_000 in
    let buf = create (1 lsl 24) in
    let rec aux p1 p2 = function
      | n when n > stop + 10 ->
          if part1 then String.sub (contents buf) stop 10
          else
            Re.Str.(search_forward (regexp cnt) (contents buf)) 0
            |> string_of_int
      | n ->
          let c1, c2 = (char_to_int (nth buf p1), char_to_int (nth buf p2)) in
          add_string buf (c1 + c2 |> string_of_int);
          let next_pos p v = (p + 1 + v) mod length buf in
          aux (next_pos c1 p1) (next_pos c2 p2) (n + 1)
    in
    add_string buf "37";
    aux 0 1 0
end

let part1 () = P1.main ~part1:true (cnt ())
let part2 () = P1.main ~part1:false (cnt ())
