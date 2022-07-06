module MyMap = Map.Make (struct
  type t = string * string

  let compare = compare
end)

module MapNeigh = Map.Make (struct
  type t = string

  let compare = compare
end)

module MySet = Set.Make (String)

let cnt =
  Lib.read_file "15" "09" (String.split_on_char ' ')
  |> List.map (function
       | [ start; _; dest; _; dist ] -> (start, dest, dist |> int_of_string)
       | _ -> raise Lib.Invalid_input)

module P1 = struct
  let makeMap =
    List.fold_left
      (fun acc (a, b, (c : int)) ->
        MyMap.add (a, b) c acc |> MyMap.add (b, a) c)
      MyMap.empty

  let makeMapSuccessors set (map : int MyMap.t) : string list MapNeigh.t =
    MySet.fold
      (fun e acc ->
        let map1 = MyMap.filter (fun (a, _) _ -> a = e) map in
        let list =
          MyMap.fold
            (fun (a, b) _ acc1 -> if a = e then b :: acc1 else acc1)
            map1 []
        in
        MapNeigh.add e list acc)
      set MapNeigh.empty

  let makeSet (map : int MyMap.t) =
    MyMap.fold
      (fun (a, b) _ acc -> MySet.add a acc |> MySet.add b)
      map MySet.empty

  let rec buildPaths (acc, numCities) (city : string)
      (neighMap : string list MapNeigh.t) (weight : int MyMap.t) op = function
    | a when MySet.cardinal a = 0 ->
        if numCities = MapNeigh.cardinal neighMap then Int.max_int else acc
    | set ->
        let neigh =
          MapNeigh.find city neighMap
          |> List.filter (fun e ->
                 try
                   MySet.find e set |> ignore;
                   true
                 with Not_found -> false)
        in
        let l =
          List.map
            (fun elt ->
              buildPaths
                (op acc @@ MyMap.find (city, elt) weight, numCities + 1)
                elt neighMap weight op (MySet.remove elt set))
            neigh
          |> List.fold_left min Int.max_int
        in
        l

  let solve op cnt =
    let citySet = makeSet cnt in
    let rec aux acc = function
      | e when MySet.cardinal e = 0 -> acc
      | e ->
          let elt = MySet.choose e in
          aux
            (min
               (buildPaths (0, 0) elt
                  (makeMapSuccessors citySet cnt)
                  cnt op (MySet.remove elt citySet))
               acc)
            (MySet.remove elt e)
    in
    aux Int.max_int citySet

  let main = solve ( + )
end

module P2 = struct
  let main v = v |> P1.solve ( - ) |> ( * ) (-1)
end

let part1 () = P1.main (P1.makeMap cnt) |> print_int
let part2 () = P2.main (P1.makeMap cnt) |> print_int
