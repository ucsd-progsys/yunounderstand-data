
let rec clone x n = if n <= 0 then [] else x :: (clone x (n - 1));;

let padZero l1 l2 =
  let first = List.length l1 in
  let sec = List.length l2 in
  if first < sec
  then (clone 0 (sec - first)) :: l1
  else
    if first > sec then (l1, ((clone 0 (first - sec)) :: l2)) else (l1, l2);;
