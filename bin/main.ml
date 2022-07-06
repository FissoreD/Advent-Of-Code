let () =
  match Sys.argv |> Array.to_list with
  | _ :: "15" :: day :: tail -> Y2015.Switcher.switch tail day
  | _ -> raise Lib.Invalid_input