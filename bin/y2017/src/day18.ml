(* https://adventofcode.com/2017/day/18 *)

let cnt () = Lib.read_file "17" "18" (String.split_on_char ' ') |> Array.of_list

module P1 = struct
  let find tbl reg =
    if Lib.is_int reg then int_of_string reg
    else Option.value ~default:0 (Hashtbl.find_opt tbl reg)

  let bin_operation pos tbl sound op r1 r2 =
    Hashtbl.replace tbl r1 (op (find tbl r1) (find tbl r2));
    (pos + 1, tbl, sound)

  let interpr_instr (pos, tbl, sound) = function
    | "snd" :: [ x ] -> (pos + 1, tbl, find tbl x)
    | "set" :: x :: [ y ] -> bin_operation pos tbl sound (fun _ a -> a) x y
    | "add" :: x :: [ y ] -> bin_operation pos tbl sound ( + ) x y
    | "mul" :: x :: [ y ] -> bin_operation pos tbl sound ( * ) x y
    | "mod" :: x :: [ y ] -> bin_operation pos tbl sound ( mod ) x y
    | "rcv" :: [ x ] when find tbl x = 0 -> (pos + 1, tbl, sound)
    | "rcv" :: _ -> (-1, tbl, sound)
    | "jgz" :: x :: [ y ] when find tbl x > 0 -> (pos + find tbl y, tbl, sound)
    | "jgz" :: _ -> (pos + 1, tbl, sound)
    | _ -> failwith "Error"

  let main cnt =
    let rec execute_instr (pos, tbl, sound) =
      if pos >= Array.length cnt || pos = -1 then sound
      else execute_instr (interpr_instr (pos, tbl, sound) cnt.(pos))
    in
    execute_instr (0, Hashtbl.create 2048, 0)
end

module P2 = struct
  type program = {
    tbl : (string, int) Hashtbl.t;
    stdin : int Queue.t;
    stdout : int Queue.t;
    mutable i : int;
    mutable sent_value : int;
    name : int;
  }

  let incr_program_pos ?(value = 1) p =
    p.i <- p.i + value;
    true

  let bin_operation p op r1 r2 =
    let new_val = op (P1.find p.tbl r1) (P1.find p.tbl r2) in
    Hashtbl.replace p.tbl r1 new_val;
    incr_program_pos p

  let interpr_instr ({ tbl; stdin; stdout; _ } as p) = function
    | "snd" :: [ x ] ->
        Queue.push (P1.find tbl x) stdout;
        p.sent_value <- p.sent_value + 1;
        incr_program_pos p
    | "rcv" :: [ _ ] when Queue.is_empty stdin -> false
    | "rcv" :: [ x ] ->
        bin_operation p (fun _ a -> a) x (string_of_int (Queue.pop stdin))
    | "set" :: x :: [ y ] -> bin_operation p (fun _x y -> y) x y
    | "add" :: x :: [ y ] -> bin_operation p ( + ) x y
    | "mul" :: x :: [ y ] -> bin_operation p ( * ) x y
    | "mod" :: x :: [ y ] -> bin_operation p ( mod ) x y
    | "jgz" :: x :: [ y ] when P1.find tbl x > 0 ->
        incr_program_pos ~value:(P1.find tbl y) p
    | "jgz" :: _ -> incr_program_pos p
    | _ -> failwith "Error"

  let create_programs () =
    let tbl p_value =
      let tbl = Hashtbl.create 2048 in
      Hashtbl.add tbl "p" p_value;
      tbl
    in
    let i, sent_value = (0, 0) in
    let q0, q1 = (Queue.create (), Queue.create ()) in
    ( { tbl = tbl 0; stdout = q0; stdin = q1; i; sent_value; name = 0 },
      { tbl = tbl 1; stdout = q1; stdin = q0; i; sent_value; name = 1 } )

  let rec run_until_block cnt p acc =
    if interpr_instr p cnt.(p.i) then run_until_block cnt p true else acc

  let main cnt =
    let p0, p1 = create_programs () in
    let rec run () =
      let r1 = run_until_block cnt p0 false in
      let r2 = run_until_block cnt p1 false in
      if r1 || r2 then run () else p1.sent_value
    in
    run ()
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
