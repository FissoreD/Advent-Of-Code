(* https://adventofcode.com/2016/day/8 *)

type rotate = { n : int; steps : int }

type instr =
  | Rect of { w : int; h : int }
  | RotateX of rotate
  | RotateY of rotate

let cnt =
  let open Lib.List in
  let funct line =
    let s = String.split_on_char ' ' line in
    let get_rotate s =
      {
        n = String.split_on_char '=' (nth s 2) |> get 1 |> int_of_string;
        steps = nth s 4 |> int_of_string;
      }
    in
    if String.starts_with ~prefix:"rect" line then
      let s = nth s 1 |> String.split_on_char 'x' |> map int_of_string in
      Rect { w = hd s; h = nth s 1 }
    else if String.starts_with ~prefix:"rotate row" line then
      RotateY (get_rotate s)
    else RotateX (get_rotate s)
  in

  Lib.read_file "16" "08" funct

let make_screen () = Array.make_matrix 50 6 '.'

module P1 = struct
  let apply_inst arr = function
    | Rect { w; h } ->
        for i = 0 to w - 1 do
          for j = 0 to h - 1 do
            arr.(i).(j) <- '#'
          done
        done
    | RotateY { n; steps } ->
        let len = Array.length arr - 1 in
        for _ = 1 to steps do
          let last = arr.(len).(n) in
          for i = 1 to len do
            arr.(Array.length arr - i).(n) <- arr.(len - i).(n)
          done;
          arr.(0).(n) <- last
        done
    | RotateX { n; steps } ->
        let len = Array.length arr.(0) - 1 in
        for _ = 1 to steps do
          let last = arr.(n).(len) in
          for i = 1 to len do
            arr.(n).(Array.length arr.(0) - i) <- arr.(n).(len - i)
          done;
          arr.(n).(0) <- last
        done

  let update_array arr = List.iter (apply_inst arr) cnt

  let main () =
    let arr = make_screen () in
    (* List.iter (apply_inst arr) cnt; *)
    update_array arr;
    Array.map (Array.map (function '#' -> 1 | _ -> 0)) arr
    |> Array.fold_left (Array.fold_left ( + )) 0
end

module P2 = struct
  let main () =
    let arr = make_screen () in
    P1.update_array arr;
    for j = 0 to Array.length arr.(0) - 1 do
      for i = 0 to Array.length arr - 1 do
        print_char arr.(i).(j)
      done;
      print_newline ()
    done
end

let part1 () = P1.main () |> string_of_int |> print_endline
let part2 () = P2.main ()
