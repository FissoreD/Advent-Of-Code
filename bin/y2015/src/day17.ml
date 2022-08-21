(* https://adventofcode.com/2015/day/17 *)

let cnt = Lib.read_file "15" "17" int_of_string |> List.sort compare

module P1 = struct
  let goal = 150

  let rec calc goal acc l =
    if goal < acc then 0
    else if goal = acc then 1
    else
      match l with
      | [] -> 0
      | hd :: tl -> calc goal (acc + hd) tl + calc goal acc tl

  let main = calc goal 0
end

module P2 = struct
  let rec calc goal len acc l =
    if acc > goal then [ None ]
    else if goal = acc then [ Some len ]
    else
      match l with
      | [] -> [ None ]
      | hd :: tl -> calc goal (len + 1) (acc + hd) tl @ calc goal len acc tl

  let main cnt =
    let l =
      calc P1.goal 0 0 cnt |> List.filter Option.is_some |> List.map Option.get
      |> List.sort compare
    in
    List.filter (( = ) (List.hd l)) l |> List.length
end

let part1 () = P1.main cnt |> print_int
let part2 () = P2.main cnt |> print_int
