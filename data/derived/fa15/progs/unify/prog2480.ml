
let pipe fs =
  let f a x = let rec a c y = a a x in (a :: x) = fs in
  let base = x in List.fold_left f base fs;;
