
let rec digitsOfInt n =
  let numL = [] in
  if (n / 10) > 0
  then ((n mod 10) :: numL) && ((digitsOfInt n) / 10)
  else numL;;
