(* https://adventofcode.com/2018/day/5 *)

let cnt () = Lib.read_file "18" "05" Lib.string_2_char_list |> List.hd

module P1 = struct
  let dist = abs (int_of_char 'A' - int_of_char 'a')
  let can_react a b = abs (int_of_char a - int_of_char b) = dist

  let rec build_stack ~skip stack = function
    | [] -> ()
    | a :: tl when skip a -> build_stack ~skip stack tl
    | a :: tl when Stack.is_empty stack || not (can_react (Stack.top stack) a)
      ->
        Stack.push a stack;
        build_stack ~skip stack tl
    | _ :: tl ->
        Stack.pop stack |> ignore;
        build_stack ~skip stack tl

  let main ?(skip = Fun.const false) (cnt : char list) =
    let stack = Stack.create () in
    build_stack ~skip stack cnt;
    Stack.length stack
end

module P2 = struct
  let main cnt =
    let skip i e =
      let to_char = Lib.letter_pos_to_char in
      e = to_char i || e = to_char ~first_letter:'A' i
    in
    let res = ref max_int in
    for i = 0 to 25 do
      res := min !res (P1.main ~skip:(skip i) cnt)
    done;
    !res
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
