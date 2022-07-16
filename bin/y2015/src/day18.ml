(* https://adventofcode.com/15/day/18 *)

let cnt =
  let l = Lib.read_file "15" "18" Lib.string_2_char_list in
  let arr _ = Array.make (List.length @@ List.hd l) '.' in
  let mat = Array.init (List.length l) arr in
  List.iteri (fun posY -> List.iteri (fun posX v -> mat.(posY).(posX) <- v)) l;
  mat

module P1 = struct
  let neigh (maxX, maxY) (x, y) =
    List.init 3 (fun i -> List.init 3 (fun j -> (x + i - 1, y + j - 1)))
    |> List.flatten
    |> List.filter (fun (newX, newY) ->
           newX >= 0 && newX < maxX && newY >= 0 && newY < maxY
           && (x, y) <> (newX, newY))

  let next_step arr =
    let new_arr =
      Array.init (Array.length arr) (fun _ ->
          Array.make (Array.length arr.(0)) '.')
    in
    let maxX, maxY = (Array.length arr.(0), Array.length arr) in
    for y = 0 to Array.length arr - 1 do
      for x = 0 to Array.length arr.(0) - 1 do
        let n = neigh (maxX, maxY) (x, y) in
        let v = arr.(y).(x) in
        let len =
          List.filter (fun (x, y) -> arr.(y).(x) = '#') n |> List.length
        in
        if v = '#' then
          new_arr.(y).(x) <- (if len = 2 || len = 3 then '#' else '.')
        else new_arr.(y).(x) <- (if len = 3 then '#' else '.')
      done
    done;
    new_arr

  let rec main arr = function
    | 0 ->
        let tot = ref 0 in
        for y = 0 to Array.length arr - 1 do
          for x = 0 to Array.length arr.(0) - 1 do
            if arr.(y).(x) = '#' then incr tot
          done
        done;
        !tot
    | n -> main (next_step arr) (n - 1)
end

module P2 = struct
  let rec main arr = function
    | 0 ->
        let tot = ref 0 in
        for y = 0 to Array.length arr - 1 do
          for x = 0 to Array.length arr.(0) - 1 do
            if arr.(y).(x) = '#' then incr tot
          done
        done;
        !tot
    | n ->
        let new_arr = P1.next_step arr in
        new_arr.(0).(0) <- '#';
        new_arr.(0).(Array.length new_arr.(0) - 1) <- '#';
        new_arr.(Array.length new_arr - 1).(0) <- '#';

        new_arr.(Array.length new_arr - 1).(Array.length new_arr.(0) - 1) <- '#';
        main new_arr (n - 1)
end

let part1 () = P1.main cnt 100 |> print_int
let part2 () = P2.main cnt 100 |> print_int
