
let rec clone x n =
  if n < 1 then [] else if n = 1 then [x] else x :: ((clone x n) - 1);;
