
let pipe fs =
  let f a x = a (fun x  -> x) in let base f = f in List.fold_left f base fs;;
