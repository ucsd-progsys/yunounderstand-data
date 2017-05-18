
let rec digitsOfInt n =
  if n <= 0 then [] else if n > 9 then (n mod 10) :: (digitsOfInt (n / 10));;
