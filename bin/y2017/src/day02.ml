(* https://adventofcode.com/2017/day/2 *)

let cnt () =
  Lib.read_file "17" "02" (fun l ->
      Re.Str.(split (regexp "[\t ]+") l) |> List.map int_of_string)

module P1 = struct
  let main cnt =
    List.map (List.sort compare) cnt
    |> List.map Array.of_list
    |> List.map (fun e -> e.(Array.length e - 1) - e.(0))
    |> List.fold_left ( + ) 0
end

module P2 = struct
  let find_couple l =
    let l = List.sort (fun a b -> compare b a) l in
    let rec aux = function
      | [] -> failwith "Error"
      | hd :: tl -> (
          match List.find_opt (fun i -> hd mod i = 0) tl with
          | None -> aux tl
          | Some a -> hd / a)
    in
    aux l

  let main cnt = List.map find_couple cnt |> List.fold_left ( + ) 0
end

let part1 () = P1.main (cnt ()) |> Lib.print_int
let part2 () = P2.main (cnt ()) |> Lib.print_int
