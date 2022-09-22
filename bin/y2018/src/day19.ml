(* https://adventofcode.com/2018/day/19 *)

let cnt () = Lib.read_file "18" "19" (String.split_on_char ' ')

module P1 = struct
  let instrs = Day16.P1.instrs

  let parse_cnt cnt =
    let pointer = int_of_string List.(nth (hd cnt) 1) in
    let rules =
      List.(map (fun l -> (hd l, -1 :: map int_of_string (tl l))) (tl cnt))
    in
    (pointer, rules |> Array.of_list)

  let apply_rule (instr, rule) reg = (List.assoc instr instrs) reg rule

  let rec run p reg rules =
    let incr reg = List.mapi (fun pos e -> e + if pos = p then 1 else 0) reg in
    let get reg = List.nth reg p in
    if get reg = 1 then reg
    else run p (incr (apply_rule rules.(get reg) reg)) rules

  let sum_of_divisors goal =
    let stop = int_of_float (sqrt (float goal)) in
    let rec aux acc = function
      | n when n > stop -> acc
      | n -> aux (acc + if goal mod n = 0 then n + (goal / n) else 0) (n + 1)
    in
    aux 0 1

  (** The program returns the sum of divisors of the value stored in R3  *)
  let main ~reg0 cnt =
    let pointer, rules = parse_cnt cnt in
    let reg = run pointer (List.init 6 (function 0 -> reg0 | _ -> 0)) rules in
    sum_of_divisors (List.nth reg 3)
end

let part1 () = P1.main ~reg0:0 (cnt ()) |> string_of_int
let part2 () = P1.main ~reg0:1 (cnt ()) |> string_of_int
