(* https://adventofcode.com/2016/day/25 *)
open Lib.List

type memory = { a : int; b : int; c : int; d : int; i : int }

let operations =
  lazy
    (let incr_try mem f =
       let mem = try Lazy.force f with _ -> mem in
       (None, { mem with i = mem.i + 1 })
     in
     let get m = function
       | "a" -> m.a
       | "b" -> m.b
       | "c" -> m.c
       | "d" -> m.d
       | _ -> invalid_arg "Error"
     in
     let to_int mem v = if Lib.is_int v then int_of_string v else get mem v in
     let set mem v = function
       | "a" -> { mem with a = v }
       | "b" -> { mem with b = v }
       | "c" -> { mem with c = v }
       | "d" -> { mem with d = v }
       | _ -> invalid_arg "Error"
     in
     [
       ( "out",
         fun (x, _) _ mem -> (Some (to_int mem x), { mem with i = mem.i + 1 })
       );
       ( "inc",
         fun (x, _) _ mem -> incr_try mem @@ lazy (set mem (get mem x + 1) x) );
       ( "dec",
         fun (x, _) _ mem -> incr_try mem @@ lazy (set mem (get mem x - 1) x) );
       ( "cpy",
         fun (x, y) _ mem -> incr_try mem @@ lazy (set mem (to_int mem x) y) );
       ( "mul",
         fun (x, y) _ mem ->
           incr_try mem @@ lazy (set mem (to_int mem x * get mem y) y) );
       ( "add",
         fun (x, y) _ mem ->
           incr_try mem @@ lazy (set mem (to_int mem x + get mem y) y) );
       ( "jnz",
         fun (x, y) _ mem ->
           let reg_val = to_int mem x = 0 in
           (None, { mem with i = (mem.i + if reg_val then 1 else to_int mem y) })
       );
       ("skip", fun (_, _) _ mem -> (None, { mem with i = mem.i + 1 }));
     ])

let row_to_instr l =
  (assoc (List.hd l) (Lazy.force operations))
    (match hd l with
    | "inc" | "dec" | "out" -> (nth l 1, "")
    | "mul" | "add" | "cpy" | "jnz" -> (nth l 1, nth l 2)
    | "skip" -> ("", "")
    | x -> invalid_arg @@ "Error not in list" ^ x)

let cnt () = Lib.read_file "16" "25" (String.split_on_char ' ') |> Array.of_list

module P1 = struct
  let replace_with_mul start =
    let new_cnt = Array.init (Array.length start) (fun i -> start.(i)) in
    let mul = [| "cpy"; "inc"; "dec"; "jnz"; "dec"; "jnz" |] in
    for i = 0 to Array.length new_cnt - 1 do
      try
        Array.iteri
          (fun pos e -> if hd new_cnt.(i + pos) <> e then raise Exit)
          mul;
        if
          nth new_cnt.(i) 2 = nth new_cnt.(i + 2) 1
          && nth new_cnt.(i) 2 = nth new_cnt.(i + 3) 1
          && nth new_cnt.(i + 4) 1 = nth new_cnt.(i + 5) 1
        then (
          let factor = nth new_cnt.(i) 1 in
          let mul_obj = nth new_cnt.(i + 5) 1 in
          let into = nth new_cnt.(i + 1) 1 in
          new_cnt.(i) <- [ "mul"; factor; mul_obj ];
          new_cnt.(i + 1) <- [ "add"; mul_obj; into ];
          new_cnt.(i + 2) <- [ "cpy"; "0"; mul_obj ];
          new_cnt.(i + 3) <- [ "cpy"; "0"; nth start.(i) 2 ];
          new_cnt.(i + 4) <- [ "skip" ];
          new_cnt.(i + 5) <- [ "skip" ])
      with _ -> ()
    done;
    new_cnt

  let main cnt =
    let len = Array.length cnt in
    let rec aux n =
      print_string @@ Printf.sprintf "\n%d " n;
      let rec aux1 stop wanted m =
        if stop = 1000 then failwith "END";
        if m.i >= len || m.i < 0 then m.a
        else
          let output, mem = row_to_instr cnt.(m.i) cnt m in
          if output = None then aux1 stop wanted mem
          else (
            print_int @@ Option.get output;
            if output = Some wanted then
              aux1 (stop + 1)
                (if output = None then wanted else (wanted + 1) mod 2)
                mem
            else failwith "Stop")
      in
      try aux1 0 0 { a = n; b = 0; c = 0; d = 0; i = 0 }
      with Failure a -> if a = "END" then n else aux (n + 1)
    in
    print_endline "";
    aux 0
end

let part1 () = P1.main (cnt ()) |> Lib.print_int
let part2 () = P1.main (cnt ()) |> Lib.print_int
