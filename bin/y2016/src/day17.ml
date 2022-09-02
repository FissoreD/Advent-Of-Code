(* https://adventofcode.com/2016/day/17 *)

let cnt () = Lib.read_file "16" "17" Lib.id |> List.hd

type position = { x : int; y : int }
type dir = U | D | L | R

module P1 = struct
  let pos_list = [ U; D; L; R ]
  let pos_to_str = function U -> "U" | D -> "D" | L -> "L" | R -> "R"
  let print_pos { x; y } = print_endline @@ Printf.sprintf "%d %d" x y

  let move { x; y } = function
    | U -> { x; y = y - 1 }
    | D -> { x; y = y + 1 }
    | L -> { x = x - 1; y }
    | R -> { x = x + 1; y }

  let is_valid { x; y } =
    let ibi = Lib.in_bouond_inclusive in
    ibi (0, 3) x && ibi (0, 3) y

  let is_open = function 'b' | 'c' | 'd' | 'e' | 'f' -> true | _ -> false
  let start_pos, finish_pos = ({ x = 0; y = 0 }, { x = 3; y = 3 })

  let hash_to_open prev_pc next_pc =
    let cat = prev_pc ^ next_pc in
    let x = Lib.md5_to_hex cat in
    List.mapi (fun pos e -> if is_open x.[pos] then Some e else None) pos_list
    |> List.filter_map Lib.id

  let main cnt =
    let to_explore = Queue.create () in
    let current_pos = ref ({ x = 0; y = 0 }, "") in
    Queue.add !current_pos to_explore;
    while fst !current_pos <> finish_pos do
      current_pos := Queue.pop to_explore;
      let pos, str = !current_pos in
      let l = hash_to_open cnt str in
      List.iter
        (fun dir ->
          let pos = move pos dir in
          if is_valid pos then Queue.add (pos, str ^ pos_to_str dir) to_explore)
        l
    done;
    snd !current_pos
end

module P2 = struct
  let main cnt =
    let q = Queue.create () in
    let current_pos = ref ({ x = 0; y = 0 }, "") in
    Queue.add !current_pos q;
    let longest_valid = ref 0 in
    while Queue.is_empty q |> not do
      current_pos := Queue.pop q;
      let pos, str = !current_pos in
      if pos = P1.finish_pos then longest_valid := String.length str
      else
        let l = P1.hash_to_open cnt str in
        List.iter
          (fun dir ->
            let pos = P1.move pos dir in
            if P1.is_valid pos then Queue.add (pos, str ^ P1.pos_to_str dir) q)
          l
    done;
    !longest_valid
end

let part1 () = P1.main (cnt ()) |> print_endline
let part2 () = P2.main (cnt ()) |> Printf.printf "%d\n"
