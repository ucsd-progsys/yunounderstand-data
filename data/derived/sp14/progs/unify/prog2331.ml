
let rec fold_l f base xs =
  match xs with | [] -> base | h::t -> fold_l (f h) base t;;