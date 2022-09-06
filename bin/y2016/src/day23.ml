(* https://adventofcode.com/2016/day/23 *)
open Lib.List

type memory = { a : int; b : int; c : int; d : int; i : int }

let operations =
  Lazy.from_val
  @@
  let incr_try mem f =
    let mem = try Lazy.force f with _ -> mem in
    { mem with i = mem.i + 1 }
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
    ("inc", fun (x, _) _ mem -> incr_try mem @@ lazy (set mem (get mem x + 1) x));
    ("dec", fun (x, _) _ mem -> incr_try mem @@ lazy (set mem (get mem x - 1) x));
    ("cpy", fun (x, y) _ mem -> incr_try mem @@ lazy (set mem (to_int mem x) y));
    ( "mul",
      fun (x, y) _ mem ->
        incr_try mem @@ lazy (set mem (to_int mem x * get mem y) y) );
    ( "add",
      fun (x, y) _ mem ->
        incr_try mem @@ lazy (set mem (to_int mem x + get mem y) y) );
    ( "jnz",
      fun (x, y) _ mem ->
        { mem with i = (mem.i + if to_int mem x = 0 then 1 else to_int mem y) }
    );
    ("skip", fun (_, _) _ mem -> { mem with i = mem.i + 1 });
    ( "tgl",
      fun (i, _) arr mem ->
        let i = mem.i + to_int mem i in
        if i >= Array.length arr then ()
        else if length arr.(i) = 2 then
          if hd arr.(i) = "inc" then arr.(i) <- "dec" :: tl arr.(i)
          else arr.(i) <- "inc" :: tl arr.(i)
        else if length arr.(i) = 3 then
          if hd arr.(i) = "jnz" then arr.(i) <- "cpy" :: tl arr.(i)
          else arr.(i) <- "jnz" :: tl arr.(i);
        { mem with i = mem.i + 1 } );
  ]

let row_to_instr l =
  (assoc (List.hd l) (Lazy.force operations))
    (match hd l with
    | "inc" | "dec" | "tgl" -> (nth l 1, "")
    | "mul" | "add" | "cpy" | "jnz" -> (nth l 1, nth l 2)
    | "skip" -> ("", "")
    | x -> invalid_arg @@ "Error not in list" ^ x)

let cnt () = Lib.read_file "16" "23" (String.split_on_char ' ') |> Array.of_list

module P1 = struct
  let main ?(m = { a = 7; b = 0; c = 0; d = 0; i = 0 }) cnt =
    let len = Array.length cnt in
    let rec aux m =
      if m.i >= len || m.i < 0 then m.a else aux (row_to_instr cnt.(m.i) cnt m)
    in
    aux m
end

module P2 = struct
  let replace_with_mul start =
    let start_copy = Array.init (Array.length start) (fun i -> start.(i)) in
    let mul = [| "cpy"; "inc"; "dec"; "jnz"; "dec"; "jnz" |] in
    for i = 0 to Array.length start_copy - 1 do
      try
        Array.iteri
          (fun pos e -> if hd start_copy.(i + pos) <> e then raise Exit)
          mul;
        if
          nth start_copy.(i) 2 = nth start_copy.(i + 2) 1
          && nth start_copy.(i) 2 = nth start_copy.(i + 3) 1
          && nth start_copy.(i + 4) 1 = nth start_copy.(i + 5) 1
        then (
          let factor = nth start_copy.(i) 1 in
          let mul_obj = nth start_copy.(i + 5) 1 in
          let into = nth start_copy.(i + 1) 1 in
          start_copy.(i) <- [ "mul"; factor; mul_obj ];
          start_copy.(i + 1) <- [ "add"; mul_obj; into ];
          start_copy.(i + 2) <- [ "cpy"; "0"; mul_obj ];
          start_copy.(i + 3) <- [ "cpy"; "0"; nth start.(i) 2 ];
          start_copy.(i + 4) <- [ "skip" ];
          start_copy.(i + 5) <- [ "skip" ])
      with _ -> ()
    done;
    start_copy

  let main cnt =
    P1.main ~m:{ a = 12; b = 0; c = 0; d = 0; i = 0 } (replace_with_mul cnt)
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
