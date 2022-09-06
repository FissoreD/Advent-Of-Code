(* https://adventofcode.com/2016/day/10 *)
open Re.Str
open Lib.Array

(* low and high bool is true if the bot gives to a bot *)
type botGives = { bot_nb : int; low : bool * int; high : bool * int }
type valueGoes = { bot_nb : int; cnt : int }
type instr = BotGives of botGives | ValueGoes of valueGoes
type bots = (bool * int) list

let cnt () =
  let to_int = int_of_string in
  let funct l =
    if String.starts_with ~prefix:"bot" l then (
      let regex =
        regexp @@ {|bot \([0-9]+\) gives low to \(.*\) |}
        ^ {|\([0-9]+\) and high to \(.*\) \([0-9]+\)|}
      in
      search_forward regex l 0 |> ignore;
      BotGives
        {
          bot_nb = matched_group 1 l |> to_int;
          low = (matched_group 2 l = "bot", matched_group 3 l |> to_int);
          high = (matched_group 4 l = "bot", matched_group 5 l |> to_int);
        })
    else (
      search_forward (regexp {|value \([0-9]+\) goes to bot \([0-9]+\)|}) l 0
      |> ignore;
      ValueGoes
        {
          cnt = matched_group 1 l |> to_int;
          bot_nb = matched_group 2 l |> to_int;
        })
  in
  Lib.read_file "16" "10" funct

module P1 = struct
  let split_cnt cnt =
    let a, b =
      List.partition (function BotGives _ -> true | _ -> false) cnt
    in
    ( List.map (function BotGives a -> a | _ -> raise Exit) a,
      List.map (function ValueGoes a -> a | _ -> raise Exit) b )

  let rec build_bots arr = function
    | [] -> arr
    | { bot_nb; cnt } :: tl ->
        arr.(bot_nb) <- append arr.(bot_nb) [| cnt |];
        build_bots arr tl

  let find_total_bot_number a b =
    max
      (List.map (function { bot_nb; _ } -> bot_nb) b
      |> List.fold_left max 0 |> ( + ) 1)
      (List.map
         (function
           | { bot_nb; low; high } -> max bot_nb (max (snd low) (snd high)))
         a
      |> List.fold_left max 0 |> ( + ) 1)

  let main ?(c1 = 17) ?(c2 = 61) cnt : int =
    let a, b = split_cnt cnt in
    let total_bot_number = find_total_bot_number a b in
    let bots = build_bots (init total_bot_number (Fun.const [||])) b in
    let outputs = make total_bot_number 0 in
    let bots_with_two_micros = findi (fun e -> length e = 2) in
    let rec aux (cnt_bot : int) =
      if mem c1 bots.(cnt_bot) && mem c2 bots.(cnt_bot) then cnt_bot
      else
        let { low; high; _ } =
          List.find (fun ({ bot_nb; _ } : botGives) -> bot_nb = cnt_bot) a
        in
        sort compare bots.(cnt_bot);
        if fst low then
          bots.(snd low) <- append bots.(snd low) [| bots.(cnt_bot).(0) |]
        else outputs.(snd low) <- outputs.(snd low) + bots.(cnt_bot).(0);
        if fst high then
          bots.(snd high) <- append bots.(snd high) [| bots.(cnt_bot).(1) |]
        else outputs.(snd high) <- outputs.(snd high) + bots.(cnt_bot).(1);
        bots.(cnt_bot) <- [||];
        try aux (bots_with_two_micros bots)
        with Not_found -> outputs.(0) * outputs.(1) * outputs.(2)
    in
    aux (bots_with_two_micros bots)
end

module P2 = struct
  let main = P1.main ~c1:(-1) ~c2:(-1)
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
