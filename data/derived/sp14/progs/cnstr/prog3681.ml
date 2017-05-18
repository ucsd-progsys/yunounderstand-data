
let rec digitsOfInt n =
  let int list digInt = (n mod 10) :: digInt in
  if (n / 10) <> 0 then digInt :: (digitsOfInt (n / 10));;
