let is1 = function
  | [ "2" ] -> false
  | [ "1" ] -> true
  | _ -> invalid_arg "Third parameters should be 1 or 2"

let switch pb = function
  | "1" -> Day01.(if is1 pb then part1 else part2) ()
  | _ -> raise Lib.Switch_not_implemented
