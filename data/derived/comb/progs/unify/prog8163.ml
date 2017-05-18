
let x l = List.map string_of_int;;

let pipe fs = let f a x = a x in let base f = x in List.fold_left f base fs;;
