
let pipe fs =
  let f a x b = x a b in let base c = c in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + 3); (fun x  -> x + x)] 3;;
