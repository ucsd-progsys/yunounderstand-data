
let pipe fs y = let f a x = x a in let base x = x in List.fold_left f base fs;;

let _ =
  pipe [(fun x  -> x ^ (", " ^ x)); (fun x  -> x ^ (", " ^ (x ^ "!")))]
    "corn";;