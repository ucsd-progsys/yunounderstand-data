
let pipe fs =
  let f a x result = a x in let base = f 0 in List.fold_left f base fs;;