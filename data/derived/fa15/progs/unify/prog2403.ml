
let sqsum xs =
  let f a x = match x with | [] -> a | x::t -> x * x in
  let base = 0 in List.fold_left f base xs;;

let _ = sqsum [1; 2; 3; 4];;
