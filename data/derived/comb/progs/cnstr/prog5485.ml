
let pipe fs =
  let f a x = match fs with | h::t -> h in
  let base = [] in List.fold_left f base fs;;
