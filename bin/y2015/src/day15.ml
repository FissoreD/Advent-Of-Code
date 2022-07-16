(* https://adventofcode.com/2015/day/15 *)

let cnt =
  Lib.read_file "15" "15" (String.split_on_char ' ')
  |> List.map (fun sub ->
         sub
         |> List.filteri (fun i _ ->
                match i with 2 | 4 | 6 | 8 | 10 -> true | _ -> false)
         |> List.map (Re.Str.global_replace (Re.Str.regexp ",") "")
         |> List.map int_of_string)

type tree = Node of (int * tree list) | End

module P1 = struct
  let rec print_tree = function
    | End -> "End"
    | Node (a, b) ->
        string_of_int a ^ "("
        ^ List.fold_left ( ^ ) "" (List.map print_tree b)
        ^ ")"

  let build_tree max_total start max_height =
    let rec aux height acc value =
      if acc = max_total then Node (value, [ End ])
      else if height < max_height - 1 then
        Node
          ( value,
            List.init (max_total - acc + 1) Lib.id
            |> List.map (fun e -> aux (height + 1) (acc + e) e) )
      else Node (value, [ Node (max_total - acc, [ End ]) ])
    in
    aux 1 start start

  let rec myMap2 op (l1 : int list) (l2 : int list list) =
    match (l1, l2) with
    | [], [] -> []
    | [], h1 :: tl -> op 0 h1 :: myMap2 op [] tl
    | _ :: _, [] -> invalid_arg "Error"
    | h1 :: t1, h2 :: t2 -> op h1 h2 :: myMap2 op t1 t2

  let calc_op (accList : int list) (cnt : int list list) =
    myMap2 (fun e f -> List.map (( * ) e) f) accList cnt
    |> List.fold_left
         (fun acc e -> List.map2 (fun a b -> a + b) acc e)
         (List.init (List.length (List.hd cnt)) (fun _ -> 0))
    |> List.fold_left (fun acc e -> acc * max 0 e) 1

  let rec find_max_comb cnt (comb : tree) (accList : int list) =
    match comb with
    | End -> calc_op (accList |> List.rev) cnt
    | Node (a, b) ->
        List.fold_left max 0
          (List.map (fun e -> find_max_comb cnt e (a :: accList)) b)

  let all_trees height max_total =
    List.init (max_total + 1) (fun e -> build_tree max_total e height)

  let main cnt =
    all_trees (List.length cnt) 100
    |> List.map (fun e -> find_max_comb cnt e [])
    |> List.fold_left max 0
end

module P2 = struct
  let calc_op (accList : int list) (cnt : int list list) =
    let res = P1.myMap2 (fun e f -> List.map (( * ) e) f) accList cnt in
    let res1 =
      res
      |> List.fold_left
           (fun acc e -> List.map2 (fun a b -> a + b) acc e)
           (List.init (List.length (List.hd cnt)) (fun _ -> 0))
    in
    if List.nth res1 (List.length res1 - 1) = 500 then
      res1 |> List.rev |> List.tl
      |> List.fold_left (fun acc e -> acc * max 0 e) 1
    else 0

  let rec find_max_comb cnt (comb : tree) (accList : int list) =
    match comb with
    | End -> calc_op (accList |> List.rev) cnt
    | Node (a, b) ->
        List.fold_left max 0
          (List.map (fun e -> find_max_comb cnt e (a :: accList)) b)

  let main cnt =
    P1.all_trees (List.length cnt) 100
    |> List.map (fun e -> find_max_comb cnt e [])
    |> List.fold_left max 0
end

let part1 () =
  P1.main
    (List.map
       (function
         | a :: b :: c :: d :: e :: _ -> [ a; b; c; d; e ]
         | _ -> invalid_arg "Error")
       cnt)
  |> print_int

let part2 () = P2.main cnt |> print_int
