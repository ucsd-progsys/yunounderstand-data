
let x l = List.map string_of_int;;

let pipe fs = let f a x a = a x in let base f = x in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + 3); (fun x  -> x + 3)] 3;;
