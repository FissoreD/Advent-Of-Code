(* https://adventofcode.com/2018/day/3 *)

let cnt () =
  Lib.read_file "18" "03" (fun l ->
      match Lib.find_all_ints_in_string l with
      | [ a; x; y; w; h ] -> (a, (x, y), (w, h))
      | _ -> failwith "Y18-D03 Invalid input")

module P1 = struct
  let find tbl k = Option.value ~default:0 @@ Hashtbl.find_opt tbl k
  let add_one tbl pos = Hashtbl.replace tbl pos (find tbl pos + 1)

  let main cnt =
    let tbl = Hashtbl.create (1 lsl 22) in
    List.iter
      (fun (_, (x, y), (w, h)) ->
        for i = x to x + w - 1 do
          for j = y to y + h - 1 do
            add_one tbl (i, j)
          done
        done)
      cnt;
    Hashtbl.to_seq_values tbl
    |> Seq.fold_left (fun acc e -> acc + if e > 1 then 1 else 0) 0
end

module P2 = struct
  let main cnt =
    let valid = Array.init (List.length cnt + 1) Fun.id in
    let tbl = Hashtbl.create (1 lsl 22) in
    List.iter
      (fun (id, (x, y), (w, h)) ->
        for i = x to x + w - 1 do
          for j = y to y + h - 1 do
            match Hashtbl.find_opt tbl (i, j) with
            | None -> Hashtbl.replace tbl (i, j) id
            | Some a ->
                valid.(a) <- 0;
                valid.(id) <- 0
          done
        done)
      cnt;
    Array.find_opt (( <> ) 0) valid |> Option.get
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
