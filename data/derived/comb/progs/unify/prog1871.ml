
let pipe fs = let f a x y = x a in let base x = x in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + x); (fun x  -> x + 3)] 3;;
