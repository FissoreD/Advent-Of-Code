(* https://adventofcode.com/2015/day/13 *)

let cnt () =
  Lib.read_file "15" "13" (fun f ->
      Re.Str.replace_first (Re.Str.regexp "\\.") "" f
      |> String.split_on_char ' ')

module T = struct
  type t = string * string

  let compare = compare
end

module MapHappiness = Map.Make (T)
module SetPerson = Set.Make (String)

module P1 = struct
  let parse_line (map, set) = function
    | [ name1; _; "gain"; how_many; _; _; _; _; _; _; name2 ] ->
        ( MapHappiness.add (name1, name2) (how_many |> int_of_string) map,
          SetPerson.add name1 set |> SetPerson.add name2 )
    | [ name1; _; "lose"; how_many; _; _; _; _; _; _; name2 ] ->
        ( MapHappiness.add (name1, name2)
            (how_many |> int_of_string |> ( * ) (-1))
            map,
          SetPerson.add name1 set |> SetPerson.add name2 )
    | _ -> raise Lib.Invalid_input

  let parse_file =
    List.fold_left parse_line (MapHappiness.empty, SetPerson.empty)

  let give_happiness map a b =
    let f = MapHappiness.find in
    f (a, b) map + f (b, a) map

  let rec create_cycle ?(acc = 0) first prev map_happ = function
    | s when SetPerson.cardinal s = 1 ->
        let x = SetPerson.choose s in
        acc + give_happiness map_happ prev x + give_happiness map_happ x first
    | set ->
        let open SetPerson in
        fold
          (fun name acc_fold ->
            max acc_fold
              (create_cycle
                 ~acc:(acc + give_happiness map_happ prev name)
                 first name map_happ (remove name set)))
          set Int.min_int

  let main cnt =
    let map, set = parse_file cnt in
    let x = SetPerson.choose set in
    create_cycle x x map (SetPerson.remove x set)
end

module P2 = struct
  let main cnt =
    let _, set = P1.parse_file cnt in
    let rec aux acc = function
      | [] -> acc @ cnt
      | hd :: tl ->
          aux
            (("XXX would gain 0 happiness units by sitting next to " ^ hd
             |> String.split_on_char ' ')
            :: (hd ^ " would gain 0 happiness units by sitting next to XXX"
               |> String.split_on_char ' ')
            :: acc)
            tl
    in
    aux [] (SetPerson.elements set) |> P1.main
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
