(* https://adventofcode.com/2017/day/8 *)

let cnt () = Lib.read_file "17" "08" (String.split_on_char ' ')

module P1 = struct
  open Hashtbl

  let reg_map =
    [
      (">", ( > ));
      ("<", ( < ));
      (">=", ( >= ));
      ("<=", ( <= ));
      ("==", ( = ));
      ("!=", ( <> ));
    ]

  let find_opt tbl r = match find_opt tbl r with None -> 0 | Some a -> a
  let i = int_of_string

  (* ev dec -705 if cag != 2 *)
  let parse_line r tbl = function
    | r1 :: op :: v1 :: "if" :: r2 :: cmp :: [ v2 ] ->
        let op = if op = "inc" then ( + ) else ( - ) in
        let cmp = List.assoc cmp reg_map in
        if cmp (find_opt tbl r2) (i v2) then (
          let old_v = find_opt tbl r1 in
          if r then remove tbl r1;
          add tbl r1 (op old_v (i v1)))
    | _ -> failwith "Invalid input (Y17-D8)"

  let main ?(r = true) cnt =
    let tbl = create 2048 in
    List.iter (parse_line r tbl) cnt;
    fold (fun _ v acc -> max acc v) tbl 0
end

let part1 () = P1.main (cnt ()) |> Lib.print_int
let part2 () = P1.main ~r:false (cnt ()) |> Lib.print_int
