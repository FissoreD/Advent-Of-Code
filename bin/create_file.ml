let file_exists filename =
  try
    Unix.access filename [ Unix.F_OK ];
    true
  with Unix.Unix_error _ -> false

let () =
  let year = Sys.argv.(1) in
  let day = Sys.argv.(2) in
  let input = "bin/y20" ^ year ^ "/input/day" ^ day ^ ".txt" in
  let src = "bin/y20" ^ year ^ "/src/day" ^ day ^ ".ml" in
  (if file_exists input |> not then
   let out_input = open_out input in
   close_out out_input);
  if file_exists src |> not then (
    let out_src = open_out src in
    Printf.fprintf out_src
      "(* https://adventofcode.com/%s/day/%s *)\n\n\
       let cnt = Lib.read_file \"%s\" \"%s\" (String.split_on_char ' ')\n"
      year day year day;
    Printf.fprintf out_src "let part1() = ()\n let part2() = ()";
    close_out out_src)
