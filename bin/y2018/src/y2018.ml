let switch nb = function
  | 1 -> Day01.(Lib.open_submodule part1 part2 nb) ()
  | 2 -> Day02.(Lib.open_submodule part1 part2 nb) ()
  | 3 -> Day03.(Lib.open_submodule part1 part2 nb) ()
  | 4 -> Day04.(Lib.open_submodule part1 part2 nb) ()
  (* | 5 -> Day05.(Lib.open_submodule part1 part2 nb) () *)
  (* | 6 -> Day06.(Lib.open_submodule part1 part2 nb) () *)
  (* | 7 -> Day07.(Lib.open_submodule part1 part2 nb) () *)
  (* | 8 -> Day08.(Lib.open_submodule part1 part2 nb) () *)
  (* | 9 -> Day09.(Lib.open_submodule part1 part2 nb) () *)
  (* | 10 -> Day10.(Lib.open_submodule part1 part2 nb) () *)
  (* | 11 -> Day11.(Lib.open_submodule part1 part2 nb) () *)
  (* | 12 -> Day12.(Lib.open_submodule part1 part2 nb) () *)
  (* | 13 -> Day13.(Lib.open_submodule part1 part2 nb) () *)
  (* | 14 -> Day14.(Lib.open_submodule part1 part2 nb) () *)
  (* | 15 -> Day15.(Lib.open_submodule part1 part2 nb) () *)
  (* | 16 -> Day16.(Lib.open_submodule part1 part2 nb) () *)
  (* | 17 -> Day17.(Lib.open_submodule part1 part2 nb) () *)
  (* | 18 -> Day18.(Lib.open_submodule part1 part2 nb) () *)
  (* | 19 -> Day19.(Lib.open_submodule part1 part2 nb) () *)
  (* | 20 -> Day20.(Lib.open_submodule part1 part2 nb) () *)
  (* | 21 -> Day21.(Lib.open_submodule part1 part2 nb) () *)
  (* | 22 -> Day22.(Lib.open_submodule part1 part2 nb) () *)
  (* | 23 -> Day23.(Lib.open_submodule part1 part2 nb) () *)
  (* | 24 -> Day24.(Lib.open_submodule part1 part2 nb) () *)
  (* | 25 -> Day25.(Lib.open_submodule part1 part2 nb) () *)
  | _ -> raise Lib.Switch_not_implemented
