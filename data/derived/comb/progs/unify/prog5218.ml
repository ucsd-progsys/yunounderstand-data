
let rec clone x n =
  match n with | 0 -> [] | n -> if n < 0 then [] else x :: (clone x (n - 1));;

let padZero l1 l2 =
  match (List.length l1) - (List.length l2) with
  | 0 -> (l1, l2)
  | n ->
      if n < 0
      then (((clone 0 (n * (-1))) @ l1), l2)
      else (((clone 0 n) @ l2), l1);;

let rec removeZero l =
  match l with | [] -> l | h::t -> if h = 0 then removeZero t else l;;

let bigAdd l1 l2 =
  let add (l1,l2) =
    let f a x =
      match (x, a) with
      | ((fst,sec),_) ->
          if (fst + sec) > 9
          then (((fst + sec) - 10), 1)
          else ((fst + sec), 0)
      | (_,(carry,digits)) ->
          if carry = 1
          then (1, (digits @ (fst, sec)))
          else (0, (digits @ (fst, sec))) in
    let base = (0, []) in
    let args = [(0, 0)] @ (List.rev (List.combine l1 l2)) in
    let (_,res) = List.fold_left f base args in res in
  removeZero (add (padZero l1 l2));;