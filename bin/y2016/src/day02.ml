(* https://adventofcode.com/2016/day/2 *)

let cnt () = Lib.read_file "16" "02" Lib.string_2_char_list

type pos = { x : int; y : int }

module P1 = struct
  let pad = [ [ "1"; "2"; "3" ]; [ "4"; "5"; "6" ]; [ "7"; "8"; "9" ] ]

  let is_valid { x; y } =
    Lib.(in_bouond_inclusive (0, 2) x && in_bouond_inclusive (0, 2) y)

  let next_pos { x; y } pos =
    let pos =
      match pos with
      | 'U' -> { x; y = y - 1 }
      | 'D' -> { x; y = y + 1 }
      | 'R' -> { x = x + 1; y }
      | _ -> { x = x - 1; y }
    in
    if is_valid pos then pos else { x; y }

  let main ?(next_pos = next_pos) ?(pos = ref { x = 1; y = 1 }) ?(pad = pad) cnt
      =
    List.map
      (fun e ->
        List.iter (fun e -> pos := next_pos !pos e) e;
        Lib.List.(pad |> get !pos.y |> get !pos.x))
      cnt
    |> List.fold_left ( ^ ) ""
end

module P2 = struct
  let pad =
    [
      [ "-1"; "-1"; "1"; "-1"; "-1" ];
      [ "-1"; "2"; "3"; "4"; "-1" ];
      [ "5"; "6"; "7"; "8"; "9" ];
      [ "-1"; "A"; "B"; "C"; "-1" ];
      [ "-1"; "-1"; "D"; "-1"; "-1" ];
    ]

  let is_valid { x; y } =
    Lib.(in_bouond_inclusive (0, 4) x && in_bouond_inclusive (0, 4) y)
    && List.nth pad y |> Lib.List.get x <> "-1"

  let next_pos { x; y } pos =
    let pos =
      match pos with
      | 'U' -> { x; y = y - 1 }
      | 'D' -> { x; y = y + 1 }
      | 'R' -> { x = x + 1; y }
      | _ -> { x = x - 1; y }
    in
    if is_valid pos then pos else { x; y }

  let main = P1.main ~next_pos ~pad ~pos:(ref { x = 0; y = 2 })
end

let part1 () = P1.main (cnt ())
let part2 () = P2.main (cnt ())
