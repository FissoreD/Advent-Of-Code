(* https://adventofcode.com/2015/day/16 *)

module MyMap = Map.Make (String)

type sue = {
  name : string;
  cnt : (string * int) * (string * int) * (string * int);
}

let cnt =
  Lib.read_file "15" "16" (String.split_on_char ' ')
  |> List.map (function
       | _ :: name :: t1 :: a :: t2 :: b :: t3 :: c :: _ ->
           {
             name = Re.Str.replace_first (Re.Str.regexp ":") "" name;
             cnt =
               ( (t1, Lib.remove_comma a |> int_of_string),
                 (t2, Lib.remove_comma b |> int_of_string),
                 (t3, Lib.remove_comma c |> int_of_string) );
           }
       | _ -> invalid_arg "Not valid pattern")

let known_properties =
  "children: 3\n\
   cats: 7\n\
   samoyeds: 2\n\
   pomeranians: 3\n\
   akitas: 0\n\
   vizslas: 0\n\
   goldfish: 5\n\
   trees: 3\n\
   cars: 2\n\
   perfumes: 1" |> String.split_on_char '\n'
  |> List.map (fun e ->
         let spl = String.split_on_char ' ' e in
         (List.hd spl, List.hd @@ List.tl spl |> int_of_string))
  |> List.fold_left (fun acc (a, b) -> MyMap.add a b acc) MyMap.empty

module P1 = struct
  let is_valid map { cnt; _ } =
    match cnt with
    | (p1, v1), (p2, v2), (p3, v3) -> (
        (try MyMap.find p1 map = v1 with Not_found -> true)
        && (try MyMap.find p2 map = v2 with Not_found -> true)
        && try MyMap.find p3 map = v3 with Not_found -> true)

  let count_valid map cnt = List.filter (is_valid map) cnt
end

module P2 = struct
  let is_valid map { cnt; _ } =
    match cnt with
    | (p1, v1), (p2, v2), (p3, v3) ->
        List.for_all
          (fun (a, b) ->
            try
              if a = "cats:" || a = "trees:" then b > MyMap.find a map
              else if a = "pomeranians:" || a = "goldfish:" then
                b < MyMap.find a map
              else MyMap.find a map = b
            with Not_found -> true)
          [ (p1, v1); (p2, v2); (p3, v3) ]

  let count_valid map cnt = List.filter (is_valid map) cnt
end

let part1 () =
  P1.count_valid known_properties cnt
  |> List.iter (function { name; _ } -> print_string name)

let part2 () =
  P2.count_valid known_properties cnt
  |> List.iter (function { name; _ } -> print_string name)
