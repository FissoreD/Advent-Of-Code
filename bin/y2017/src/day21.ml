(* https://adventofcode.com/2017/day/21 *)

let cnt () =
  let split_string s =
    String.split_on_char '/' s |> List.map Lib.string_2_char_list
  in
  Lib.read_file "17" "21" (fun e ->
      match Re.Str.(split (regexp " => ") e) with
      | [ a; b ] -> (split_string a, split_string b)
      | _ -> invalid_arg "Y17-D21 Invalid Input")

module P1 = struct
  let all_combo_of_row (tbl : (char array array, char array array) Hashtbl.t)
      (key, value) =
    let to_matrix_array l = l |> List.map Array.of_list |> Array.of_list in
    let value = to_matrix_array value in
    let rotate_x_axis = List.map List.rev in
    let rotate_y_axis = List.rev in
    let rotate_diag = Lib.List.transpose_matrix in
    let compose f g x = f (g x) in
    let combos =
      [
        Fun.id;
        rotate_x_axis;
        rotate_y_axis;
        rotate_diag;
        compose rotate_x_axis rotate_y_axis;
        compose rotate_x_axis rotate_diag;
        compose rotate_diag rotate_y_axis;
        compose rotate_diag @@ compose rotate_x_axis rotate_y_axis;
      ]
    in
    List.iter (fun c -> Hashtbl.add tbl (c key |> to_matrix_array) value) combos

  let inp_to_tbl inp =
    let tbl = Hashtbl.create (List.length inp * 8) in
    List.iter (all_combo_of_row tbl) inp;
    tbl

  let rec res_size = function
    | 0 -> 3
    | 1 -> 4
    | 2 -> 6
    | n -> 3 * res_size (n - 3)

  let build_matrix n = ref (Array.make_matrix n n '.')

  let update_matrix tbl inp_mat out_mat x y size =
    let sub_matrix =
      Array.(
        sub inp_mat (y * size) size |> map (fun e -> sub e (x * size) size))
    in
    let res = Hashtbl.find tbl sub_matrix in
    for y1 = 0 to size do
      for x1 = 0 to size do
        out_mat.((y * (size + 1)) + y1).((x * (size + 1)) + x1) <- res.(y1).(x1)
      done
    done

  let main ?(times = 5) cnt =
    let tbl, res_size = (inp_to_tbl cnt, res_size times) in
    let m1, m2 = (build_matrix res_size, build_matrix res_size) in
    let grid_size = ref 3 in

    Array.iteri
      (fun y e -> Array.iteri (fun x e -> !m1.(y).(x) <- e) e)
      [| [| '.'; '#'; '.' |]; [| '.'; '.'; '#' |]; [| '#'; '#'; '#' |] |];

    for _ = 1 to times do
      let sub_grid_size = if !grid_size land 1 = 0 then 2 else 3 in
      for x = 0 to (!grid_size / sub_grid_size) - 1 do
        for y = 0 to (!grid_size / sub_grid_size) - 1 do
          update_matrix tbl !m1 !m2 x y sub_grid_size
        done
      done;
      grid_size := !grid_size / sub_grid_size * (sub_grid_size + 1);
      let prov = !m1 in
      m1 := !m2;
      m2 := prov
    done;
    Array.fold_left
      (fun acc e ->
        acc + Array.fold_left (fun acc e -> acc + Lib.bool_to_int (e = '#')) 0 e)
      0 !m1
end

let part1 () = P1.main (cnt ()) |> string_of_int
let part2 () = P1.main ~times:18 (cnt ()) |> string_of_int
