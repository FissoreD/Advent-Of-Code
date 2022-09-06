let expected =
  [|
    (* 2015 *)
    [|
      ("74", "1795");
      ("1586300", "3737498");
      ("2572", "2631");
      ("346386", "9958218");
      ("255", "55");
      ("543903", "14687245");
      ("16076", "2797");
      ("1350", "2085");
      ("141", "736");
      ("360154", "5103798");
      ("cqjxxyzz", "cqkaabcc");
      ("156366", "96852");
      ("709", "668");
      ("2655", "1059");
      ("21367368", "1766400");
      ("213", "323");
      ("654", "57");
      ("1061", "1006");
      ("509", "195");
      ("831600", "884520");
      ("121", "201");
      ("900", "1216");
      ("255", "334");
      ("11846773891", "80393059");
      ("9132360", "");
    |];
    (* 2016 *)
    [|
      ("307", "165");
      ("24862", "46C91");
      ("862", "1577");
      ("245102", "324");
      ("801b56a7", "424a0197");
      ("afwlyyyq", "bhkzekao");
      ("110", "242");
      ( "116",
        "#..#.###...##....##.####.#....###...##..####.####.\n\
         #..#.#..#.#..#....#.#....#....#..#.#..#.#.......#.\n\
         #..#.#..#.#..#....#.###..#....###..#....###....#..\n\
         #..#.###..#..#....#.#....#....#..#.#....#.....#...\n\
         #..#.#....#..#.#..#.#....#....#..#.#..#.#....#....\n\
         .##..#.....##...##..#....####.###...##..####.####.\n" );
      ("183269", "11317278863");
      ("101", "37789");
      ("47", "71");
      ("318020", "9227674");
      ("82", "138");
      ("18626", "20092");
      ("121834", "3208099");
      ("01110011101111011", "11001111011000111");
      ("DDRRUDLRRD", "488");
      ("1951", "20002936");
      ("1842613", "1424135");
      ("31053880", "117");
      ("bdfhgeca", "gdfcabeh");
      ("1024", "230");
      ("11123", "479007683");
      ("442", "660");
      ("198", "");
    |];
    (* 2017 *)
    [|
      ("1223", "1284");
      ("42299", "277");
      ("552", "330785");
      ("451", "223");
      ("354121", "27283023");
      ("12841", "8038");
      ("veboyvy", "749");
      ("5143", "6209");
      ("7640", "4368");
      ("7888", "decdf7d377879877173b7f2fb131cf1b");
      ("805", "1535");
      ("130", "189");
      ("2604", "3941460");
      ("8204", "1089");
      ("600", "313");
      ("padheomkgjfnblic", "bfcdeakhijmlgopn");
      ("1311", "39170601");
      ("7071", "8001");
    |];
  |]

let raise_exception y d p res =
  if d <> 25 || p <> 2 then
    let expected = (if p = 1 then fst else snd) expected.(y - 15).(d - 1) in
    if expected <> res then
      failwith
      @@ Printf.sprintf "Year %d day %d part %d result := %s, expected := %s"
           (2000 + y) d p res expected

let () =
  let inp = Sys.argv in
  let start_year, end_year =
    match Array.find_opt (String.starts_with ~prefix:"y") inp with
    | None -> (15, Array.length expected - 1)
    | Some s -> (int_of_string @@ String.sub s 1 2, 0)
  in
  let start_day, end_day =
    match Array.find_opt (String.starts_with ~prefix:"d") inp with
    | None -> (1, true)
    | Some s -> (int_of_string @@ String.sub s 1 (String.length s - 1), false)
  in
  for year = start_year to start_year + end_year do
    for
      day = start_day
      to start_day
         + if end_day then Array.length expected.(year - 15) - 1 else 0
    do
      print_endline
      @@ Printf.sprintf "https://adventofcode.com/20%d/day/%d" year day;
      for part = 1 to 2 do
        let time = Sys.time () in
        let res =
          match year with
          | 15 -> Y2015.switch part day
          | 16 -> Y2016.switch part day
          | 17 -> Y2017.switch part day
          | _ -> raise Lib.Invalid_input
        in
        raise_exception year day part res;

        print_endline
        @@ Printf.sprintf "Executed Year %2d - Day %2d - Part %d -> Time : %.5f"
             year day part
             (Sys.time () -. time)
      done
    done
  done
