
let pipe fs = let f a x = fs in let base = fs in List.fold_left f base fs;;

let pipe fs =
  let f a x = pipe a x in let base = 3 in List.fold_left f base fs;;