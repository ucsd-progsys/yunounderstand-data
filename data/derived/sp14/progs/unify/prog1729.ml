
let rec clone x n = if n <= 0 then [] else x :: (clone x (n - 1));;

let padZero l1 l2 =
  let diff = (List.length l2) - (List.length l1) in
  (((clone 0 diff) @ l1), ((clone 0 (- diff)) @ l2));;

let rec removeZero l =
  match l with | [] -> l | h::t -> if h = 0 then removeZero t else l;;

let bigAdd l1 l2 =
  let add (l1,l2) =
    let f a x =
      let (carry,num) = a in
      let (l1',l2') = x in
      let addit = (l1' + l2') + carry in
      Printf.printf Printf.sprintf "%8d%8d%8d\n" l1' l2' carry;
      ((if addit >= 10 then 1 else 0), (num @ [addit mod 10])) in
    let base = (0, []) in
    let args = List.combine ([0] @ l1) ([0] @ l2) in
    let (car,res) = List.fold_left f base args in res in
  removeZero (add (padZero l1 l2));;
