let switch nb = function
  | 1 -> Day01.(Lib.open_submodule part1 part2 nb) ()
  | 2 -> Day02.(Lib.open_submodule part1 part2 nb) ()
  | 3 -> Day03.(Lib.open_submodule part1 part2 nb) ()
  | 4 -> Day04.(Lib.open_submodule part1 part2 nb) ()
  | 5 -> Day05.(Lib.open_submodule part1 part2 nb) ()
  | 6 -> Day06.(Lib.open_submodule part1 part2 nb) ()
  | 7 -> Day07.(Lib.open_submodule part1 part2 nb) ()
  | _ -> raise Lib.Switch_not_implemented
