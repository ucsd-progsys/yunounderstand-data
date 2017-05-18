
let padZero l1 l2 =
  if (List.length l1) = (List.length l2) then (l1, l2) else (l1, l2);;

let padZero l1 l2 =
  if (List.length l1) = (List.length l2)
  then (l1, l2)
  else if (List.length l1) < (List.length l2) then padZero ((0 :: l1) l2);;
