
let pipe fs = let f a x = a x in let base a = f a in List.fold_left f base fs;;