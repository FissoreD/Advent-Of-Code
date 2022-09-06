(* https://adventofcode.com/2015/day/6 *)

let str_to_int_couple s =
  let l = String.split_on_char ',' s in
  match l with
  | [ a; b ] -> (int_of_string a, int_of_string b)
  | _ -> raise Lib.Invalid_input

let parse_line l =
  let line = String.split_on_char ' ' l in
  match line with
  | [ _; from; _; upto ] ->
      ("toogle", str_to_int_couple from, str_to_int_couple upto)
  | [ _; operation; from; _; upto ] ->
      (operation, str_to_int_couple from, str_to_int_couple upto)
  | _ -> raise Lib.Invalid_input

let cnt () = Lib.read_file "15" "06" parse_line

module P1 = struct
  let treat_operation = function
    | "on" -> fun _ -> true
    | "off" -> fun _ -> false
    | _ -> fun e -> not e

  let modify_bool matrix ((f : string), (a, b), (c, d)) op =
    for x = a to c do
      for y = b to d do
        matrix.(x).(y) <- op f matrix.(x).(y)
      done
    done

  let rec make_operation op matrix = function
    | [] -> ()
    | hd :: tl ->
        modify_bool matrix hd op;
        make_operation op matrix tl

  let main matrix cnt =
    make_operation treat_operation matrix cnt;
    Array.fold_left
      (fun cmp l ->
        cmp + Array.fold_left (fun a b -> a + Lib.bool_to_int b) 0 l)
      0 matrix
end

module P2 = struct
  let treat_operation s cnt =
    match s with "on" -> cnt + 1 | "off" -> max 0 (cnt - 1) | _ -> cnt + 2

  let main matrix cnt =
    P1.make_operation treat_operation matrix cnt;
    Array.fold_left (fun cmp l -> cmp + Array.fold_left ( + ) 0 l) 0 matrix
end

let x, y = (1000, 1000)

let part1 () =
  P1.main (Array.init x (fun _ -> Array.make y false)) (cnt ()) |> string_of_int

let part2 () =
  P2.main (Array.init x (fun _ -> Array.make y 0)) (cnt ()) |> string_of_int
