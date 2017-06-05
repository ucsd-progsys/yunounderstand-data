
let f a x = x a;;

let f a = f;;

let pipe fs = let f a x = f a in let base f = f in List.fold_left f base fs;;
