
let sqsum xs =
  let f a x = match x with | [] -> 0 | h::t -> a * a in
  let base = List.hd xs in List.fold_left f base xs;;
