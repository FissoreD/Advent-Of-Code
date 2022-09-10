(* https://adventofcode.com/2017/day/25 *)

let cnt () = Lib.read_file "17" "25" Fun.id |> Array.of_list

module P1 = struct
  type state_char = { state_name : string; symb : int }
  type action = { symb_to_write : int; direction : int; next_state : string }

  type tm = {
    mutable tape : (int, int) Hashtbl.t;
    mutable current_state : string;
    mutable current_pos : int;
    mutable transitions : (state_char, action) Hashtbl.t;
    step_nb : int;
  }

  let search regex str =
    let open Re.Str in
    search_forward (regexp regex) str 0 |> ignore;
    matched_group 1 str

  let build_tm (cnt : string array) =
    let ints = Lib.find_all_ints_in_string in

    let transitions = Hashtbl.create (1 lsl 10) in
    let tape = Hashtbl.create 2048 in

    let current_state = search {|Begin in state \(.+\).|} cnt.(0) in
    let step_nb = List.hd (ints cnt.(1)) in

    let index = ref 3 in
    while !index < Array.length cnt do
      let state_name = search {|In state \(.+\):|} cnt.(!index) in
      for i = 0 to 1 do
        let symb = ints cnt.(!index + 1 + (i * 4)) |> List.hd in
        let symb_to_write = ints cnt.(!index + 2 + (i * 4)) |> List.hd in
        let direction =
          if String.ends_with ~suffix:"right." cnt.(!index + 3 + (i * 4)) then 1
          else -1
        in
        let next_state = search {|.* \(.+\)\.|} cnt.(!index + 4 + (i * 4)) in
        Hashtbl.add transitions { state_name; symb }
          { direction; next_state; symb_to_write }
      done;
      index := !index + 10
    done;

    { tape; current_state; current_pos = 0; transitions; step_nb }

  let tape_find tbl k = Option.value ~default:0 (Hashtbl.find_opt tbl k)

  let main cnt =
    let tm = build_tm cnt in
    for _ = 1 to tm.step_nb do
      let { symb_to_write; direction; next_state } =
        let state_name = tm.current_state in
        let symb = tape_find tm.tape tm.current_pos in
        Hashtbl.find tm.transitions { state_name; symb }
      in
      Hashtbl.replace tm.tape tm.current_pos symb_to_write;
      tm.current_pos <- tm.current_pos + direction;
      tm.current_state <- next_state
    done;
    Seq.fold_left ( + ) 0 (Hashtbl.to_seq_values tm.tape)
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = "Nope"
