let is1 = function
  | [ "2" ] -> false
  | [ "1" ] -> true
  | _ -> invalid_arg "Third parameters should be 1 or 2"

let switch pb = function
  | "1" -> Day01.(if is1 pb then part1 else part2) ()
  | "2" -> Day02.(if is1 pb then part1 else part2) ()
  | "3" -> Day03.(if is1 pb then part1 else part2) ()
  | "4" -> Day04.(if is1 pb then part1 else part2) ()
  | "5" -> Day05.(if is1 pb then part1 else part2) ()
  | "6" -> Day06.(if is1 pb then part1 else part2) ()
  | "7" -> Day07.(if is1 pb then part1 else part2) ()
  | "8" -> Day08.(if is1 pb then part1 else part2) ()
  | "9" -> Day09.(if is1 pb then part1 else part2) ()
  | "10" -> Day10.(if is1 pb then part1 else part2) ()
  | "11" -> Day11.(if is1 pb then part1 else part2) ()
  | "12" -> Day12.(if is1 pb then part1 else part2) ()
  | "13" -> Day13.(if is1 pb then part1 else part2) ()
  | "14" -> Day14.(if is1 pb then part1 else part2) ()
  | "15" -> Day15.(if is1 pb then part1 else part2) ()
  | "16" -> Day16.(if is1 pb then part1 else part2) ()
  | "17" -> Day17.(if is1 pb then part1 else part2) ()
  | "18" -> Day18.(if is1 pb then part1 else part2) ()
  | "19" -> Day19.(if is1 pb then part1 else part2) ()
  | "20" -> Day20.(if is1 pb then part1 else part2) ()
  | "21" -> Day21.(if is1 pb then part1 else part2) ()
  | "22" -> Day22.(if is1 pb then part1 else part2) ()
  | "23" -> Day23.(if is1 pb then part1 else part2) ()
  | "24" -> Day24.(if is1 pb then part1 else part2) ()
  | "25" -> Day25.(if is1 pb then part1 else part2) ()
  | _ -> raise Lib.Switch_not_implemented
