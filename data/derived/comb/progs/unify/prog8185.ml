
let pipe fs =
  let f a x x = x a in let base f = f fs in List.fold_left f base fs;;
