
let pipe fs =
  let f a x y c = x a in let base g x = x in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + x); (fun x  -> x + 3)] 3;;
