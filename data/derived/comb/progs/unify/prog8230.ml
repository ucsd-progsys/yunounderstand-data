
let pipe fs = let f a x = x a in let base f a = a in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + x); (fun x  -> x + 3)] 3;;
