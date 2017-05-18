
let pipe fs =
  let f a x p' = x a in let base a' = a' in List.fold_left f base fs;;

let pipe fs =
  let f a x p' = x a in let base = pipe [] in List.fold_left f base fs;;

let pipe fs =
  let f a x p' = x a in let base = pipe [] in List.fold_left f base fs;;

let pipe fs =
  let f a x p' = x a in let base = pipe [] in List.fold_left f base fs;;

let pipe fs =
  let f a x p' = x a in let base = pipe [] in List.fold_left f base fs;;

let pipe fs =
  let f a x p' = x a in let base = pipe [] in List.fold_left f base fs;;

let pipe fs =
  let f a x = x a in let base = pipe [] in List.fold_left f base fs;;

let pipe fs =
  let f a x = a x in let base = pipe [] in List.fold_left f base fs;;