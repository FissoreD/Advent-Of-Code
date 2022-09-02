let switch nb = function
  | 1 -> Day01.(Lib.open_submodule part1 part2 nb) ()
  | 2 -> Day02.(Lib.open_submodule part1 part2 nb) ()
  | _ -> raise Lib.Switch_not_implemented
