
let pipe fs =
  let f a x = match fs with | h::t -> h in
  let base = 0 in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + x); (fun x  -> x + 3)] 3;;
