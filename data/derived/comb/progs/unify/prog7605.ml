
let sqsum xs =
  let f a x = a + (x * x) in let base = 0 in List.fold_right f base xs;;