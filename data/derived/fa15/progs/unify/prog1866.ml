
let pipe fs = let f a x = x a in let base = 1 in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + 3); (fun x  -> x + x)] 3;;
