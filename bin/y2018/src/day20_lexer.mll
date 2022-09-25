{ 
  open Day20_parser 
  let to_dir c = Pos.str_to_dir (String.lowercase_ascii @@ Lib.string_of_char c)
}

rule f = parse
  | "^" { f lexbuf }
  | "|" { OR }
  | "(" { OPEN_P }
  | ")" { CLOSE_P }
  | "$" { f lexbuf }
  | (("N" | "S" | "E" | "W") as c) { DIR (to_dir c) }
  | eof  { EOF }
 