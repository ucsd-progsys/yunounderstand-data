
let rec clone x n = if n > 0 then x :: (clone x (n - 1)) else [];;

let padZero l1 l2 =
  let x = List.length l1 in
  let y = List.length l2 in
  if x = y
  then (l1, l2)
  else
    if x < y
    then (((clone 0 (y - x)) @ l1), l2)
    else (l1, ((clone 0 (x - y)) @ l2));;

let rec removeZero l =
  match l with | [] -> [] | h::t -> if h = 0 then removeZero t else l;;

let bigAdd l1 l2 =
  let add (l1,l2) =
    let f a x =
      let (l1x,l2x) = x in
      let (a1,a2) = a in
      let sum = (l1x + l2x) + (a2 [0]) in (0. (sum % 10)) :: a2 in
    let base = ([], []) in
    let args = List.rev (List.combine l1 l2) in
    let (_,res) = List.fold_left f base args in res in
  removeZero (add (padZero l1 l2));;
