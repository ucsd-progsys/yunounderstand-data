
let rec clone x n = if n <= 0 then [] else x :: (clone x (n - 1));;

let padZero l1 l2 =
  let len1 = List.length l1 in
  let len2 = List.length l2 in
  if len1 > len2
  then (l1, ((clone 0 (len1 - len2)) @ l2))
  else (((clone 0 (len2 - len1)) @ l1), l2);;

let rec removeZero l =
  match l with | [] -> [] | 0::t -> removeZero t | _ -> l;;

let bigAdd l1 l2 =
  let add (l1',l2') =
    let f a x =
      let (x1,x2) = x in
      let (carry,acc) = a in
      let sum = (x1 + x2) + carry in ((sum / 10), (acc :: (sum mod 10))) in
    let base = (0, []) in
    let args = let (l1',l2') = padZero l1 l2 in List.combine l2' l1' in
    let (_,res) = List.fold_left f base args in res in
  removeZero (add (padZero l1 l2));;
