(* https://adventofcode.com/2015/day/20 *)

let cnt () = Lib.read_file "15" "20" String.trim |> List.hd |> int_of_string

module P1 = struct
  let number_presents = 10

  let give_primes n =
    let rec aux x =
      if x <= int_of_float (sqrt (float n)) then
        if n mod x = 0 then
          let y = n / x in
          if y <> x then x :: y :: aux (x + 1) else [ x ]
        else aux (x + 1)
      else []
    in
    if n <> 1 then 1 :: n :: aux 2 else [ 1 ]

  let sum_of_primes n = List.fold_left ( + ) 0 (give_primes n)
  let go_next_house house_num = sum_of_primes house_num * number_presents

  let main n =
    let rec aux cnt_house =
      let acc = go_next_house cnt_house in
      if acc >= n then cnt_house else aux (cnt_house + 1)
    in
    aux 1
end

module P2 = struct
  let max_house_explored = 51
  let number_presents = 11

  let give_primes_modif n =
    let rec aux x =
      if x <= int_of_float (sqrt (float n)) && x < max_house_explored then
        if n mod x = 0 then
          let y = n / x in
          if y <> x then
            if y < max_house_explored then x :: y :: aux (x + 1)
            else y :: aux (x + 1)
          else [ x ]
        else aux (x + 1)
      else []
    in
    if n <> 1 then 1 :: n :: aux 2 else [ 1 ]

  let sum_of_primes n = List.fold_left ( + ) 0 (give_primes_modif n)
  let go_next_house house_num = sum_of_primes house_num * number_presents

  let main n =
    let rec aux cnt_house =
      let acc = go_next_house cnt_house in
      if acc >= n then cnt_house else aux (cnt_house + 1)
    in
    aux 1
end

let part1 () = P1.main (cnt ()) |> print_int
let part2 () = P2.main (cnt ()) |> print_int
