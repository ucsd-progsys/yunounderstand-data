
let pipe fs = let f a x f a = x in let base y = y in List.fold_left f base fs;;

let _ = pipe [] 3;;