
let pipe fs = let f a x = x a in let base b c = c in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + x); (fun x  -> x + 3)] 3;;
