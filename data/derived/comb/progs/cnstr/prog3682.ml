
let rec digitsOfInt n =
  let int list digInt = [] in
  if (n / 10) <> 0 then digInt :: (digitsOfInt (n / 10));;
