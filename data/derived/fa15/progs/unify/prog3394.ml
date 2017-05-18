
let x l = List.map string_of_int;;

let pipe fs =
  let f a x x = x a in let base f = f x in List.fold_left f base fs;;
