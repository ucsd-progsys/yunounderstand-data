
let pipe fs = let f a x w = x a in let base q = q in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + x); (fun x  -> x + 3)] 3;;
