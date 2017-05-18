
let y f g x = f (g x);;

let rec mulByDigit i l =
  let f a x =
    match a with
    | [] -> (i * x) :: a
    | h::t -> [(x * y) + (h / 10); h mod 10] @ t in
  let base = [] in let args = List.rev (0 :: l) in List.fold_left f base args;;