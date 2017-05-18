
let rec filter (l,a) =
  match l with
  | [] -> []
  | hd::tl -> if hd = a then filter (tl, a) else hd :: (filter (tl, a));;

let filter l h =
  match l with
  | [] -> []
  | hd::tl -> if hd = h then hd :: (filter tl h) else filter tl h;;
