(* https://adventofcode.com/2017/day/23 *)

let cnt () = Lib.read_file "17" "23" (String.split_on_char ' ') |> Array.of_list

module P1 = struct
  type tm = {
    mem : (string, int) Hashtbl.t;
    instr_l : string list array;
    mutable instr : int;
    mutable mul_nb : int;
  }

  let find { mem; _ } k =
    if Lib.is_int k then int_of_string k
    else match Hashtbl.find_opt mem k with None -> 0 | Some a -> a

  let incr_instr tm = tm.instr <- tm.instr + 1

  let set tm k v =
    Hashtbl.replace tm.mem k (find tm v);
    incr_instr tm

  let make_op op tm k v = set tm k (string_of_int (op (find tm k) (find tm v)))

  let interp_instr tm =
    match tm.instr_l.(tm.instr) with
    | "set" :: x :: [ y ] -> set tm x y
    | "sub" :: x :: [ y ] -> make_op ( - ) tm x y
    | "mul" :: x :: [ y ] ->
        make_op ( * ) tm x y;
        tm.mul_nb <- tm.mul_nb + 1
    | "jnz" :: x :: [ _ ] when find tm x = 0 -> incr_instr tm
    | "jnz" :: _ :: [ y ] -> tm.instr <- tm.instr + find tm y
    | _ -> failwith "Error"

  let make_tm instr_l =
    { instr = 0; instr_l; mem = Hashtbl.create 8; mul_nb = 0 }

  let finish tm = tm.instr < 0 || tm.instr >= Array.length tm.instr_l

  (** Same as [ 
    let a = int_of_string (List.nth cnt.(0) 2) - 2 in a * a
    ] *)
  let main cnt =
    (* let tm = make_tm cnt in
       while not (finish tm) do
         interp_instr tm
       done;
       tm.mul_nb *)
    let a = int_of_string (List.nth cnt.(0) 2) - 2 in
    a * a
end

module P2 = struct
  include P1

  let main cnt =
    (* let tm = make_tm cnt in
       Hashtbl.replace tm.mem "a" 1;
       while not (finish tm) do
         interp_instr tm
       done;
       tm.mul_nb *)
    let hd = List.nth cnt.(0) 2 |> int_of_string in
    let h = ref 0 in
    let b = ref (100000 + (hd * 100)) in
    let stop = !b + 17000 in
    while !b <= stop do
      (try
         for d = 2 to !b - 1 do
           if !b mod d = 0 then (
             incr h;
             raise Exit)
         done
       with Exit -> ());
      b := !b + 17
    done;
    !h
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
