(* https://adventofcode.com/2018/day/16 *)

let cnt () =
  let res = Lib.read_file "18" "16" Fun.id in
  let ints = Lib.find_all_ints_in_string in
  let rec parser sample instr = function
    | a :: b :: c :: tl when String.starts_with ~prefix:"Before" a ->
        parser ((ints a, ints b, ints c) :: sample) instr tl
    | "" :: tl -> parser sample instr tl
    | hd :: tl -> parser sample (ints hd :: instr) tl
    | [] -> (sample |> List.rev, instr |> List.rev)
  in
  parser [] [] res

module P1 = struct
  let apply_rule op fst_is_reg snd_is_reg bef rule =
    let open List in
    let f a b c i e =
      if i = c then
        op
          (if fst_is_reg then nth bef a else a)
          (if snd_is_reg then nth bef b else b)
      else e
    in
    match rule with
    | [ _; a; b; c ] -> mapi (f a b c) bef
    | _ -> invalid_arg "Y18D16 Invalid input"

  let instrs =
    let to_int b = if b then 1 else 0 in
    let ( >> ) a b = to_int (a > b) in
    let ( == ) a b = to_int (a = b) in
    [
      ("addi", apply_rule ( + ) true false);
      ("addr", apply_rule ( + ) true true);
      ("muli", apply_rule ( * ) true false);
      ("mulr", apply_rule ( * ) true true);
      ("landi", apply_rule ( land ) true false);
      ("landr", apply_rule ( land ) true true);
      ("lori", apply_rule ( lor ) true false);
      ("lorr", apply_rule ( lor ) true true);
      ("seti", apply_rule (fun a _ -> a) false false);
      ("setr", apply_rule (fun a _ -> a) true true);
      ("gtrr", apply_rule ( >> ) true true);
      ("gtir", apply_rule ( >> ) false true);
      ("gtri", apply_rule ( >> ) true false);
      ("eqrr", apply_rule ( == ) true true);
      ("eqir", apply_rule ( == ) false true);
      ("eqri", apply_rule ( == ) true false);
    ]

  let find_op_codes (bef, rule, aft) =
    List.filter (fun (_, b) -> b bef rule = aft) instrs

  let main (sample, _) =
    List.fold_left
      (fun acc x -> acc + if List.length (find_op_codes x) > 2 then 1 else 0)
      0 sample
end

module P2 = struct
  module S = Set.Make (struct
    type t = string * (int list -> int list -> int list)

    let compare a b = compare (fst a) (fst b)
  end)

  let build_opcode_instr_assoc sample =
    let assoc = Hashtbl.create 16 in
    let find k =
      Option.value ~default:(S.of_list P1.instrs) (Hashtbl.find_opt assoc k)
    in
    List.iter
      (fun ((_, op, _) as x) ->
        let res = P1.find_op_codes x |> S.of_list in
        Hashtbl.replace assoc (List.hd op) (S.inter res @@ find (List.hd op)))
      sample;
    assoc

  let rec simplify res = function
    | [] -> res
    | assoc ->
        let sing, mul =
          List.partition_map
            (fun (r, s) ->
              if S.cardinal s = 1 then Left (r, s) else Right (r, s))
            assoc
        in
        let mul =
          List.map
            (fun (k, v) ->
              (k, List.fold_left (fun acc e -> S.diff acc (snd e)) v sing))
            mul
        in
        let get_func v = snd (S.choose v) in
        simplify (res @ List.map (fun (k, v) -> (k, get_func v)) sing) mul

  let main (sample, (instr : int list list)) =
    let op_code_instr_assoc =
      simplify []
        (Hashtbl.to_seq (build_opcode_instr_assoc sample) |> List.of_seq)
    in
    let mem = List.init 4 (Fun.const 0) in
    List.fold_left
      (fun acc (e : int list) ->
        (List.assoc (List.hd e) op_code_instr_assoc) acc e)
      mem instr
    |> List.hd
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P2.main (cnt ()) |> string_of_int
