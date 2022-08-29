let () =
  match Sys.argv |> Array.to_list |> List.tl |> List.map int_of_string with
  | 15 :: day :: tail -> Y2015.switch tail day
  | 16 :: day :: tail -> Y2016.switch tail day
  | _ -> raise Lib.Invalid_input