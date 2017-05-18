
let pipe fs n =
  let f a x = a + (n x) in let base = 0 in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + 3); (fun x  -> x + x)] 3;;