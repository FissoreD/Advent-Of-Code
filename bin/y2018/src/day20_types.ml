type regex =
  | Empty
  | Symbol of Pos.Dir.dirs
  | Cat of regex * regex
  | Choice of regex * regex
