(* https://adventofcode.com/2016/day/18 *)

let cnt () =
  Lib.read_file "16" "18" (fun e ->
      Array.init (String.length e) (fun i -> e.[i]))
  |> List.hd

module P1 = struct
  let trap = '^'
  let safe = '.'
  let is_safe = ( = ) safe

  let create_tile row pos len =
    let fst = if pos = 0 then safe else row.(pos - 1) in
    let snd = if pos + 1 = len then safe else row.(pos + 1) in
    if fst <> snd then trap else safe

  let create_next_row row arr len =
    for i = 0 to len - 1 do
      arr.(i) <- create_tile row i len
    done

  let count_symbols =
    Array.fold_left (fun acc e -> acc + if is_safe e then 1 else 0) 0

  let main ?(row_nb = 40) cnt =
    let len = Array.length cnt in
    let arr1, arr2 = (cnt, Array.make len safe) in
    let rec aux acc a b = function
      | 0 -> acc
      | n ->
          create_next_row a b len;
          aux (acc + count_symbols a) b a (n - 1)
    in
    aux 0 arr1 arr2 row_nb
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P1.main ~row_nb:400000 (cnt ()) |> string_of_int
