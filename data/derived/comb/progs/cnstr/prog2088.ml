
let rec digitsOfInt n =
  if n <= 0
  then []
  else
    if (n mod 10) = 0
    then 0 :: (digitsOfInt (n / 10))
    else if ((n - 1) mod 10) = 0 then 1 :: (digitsOfInt ((n - 1) / 10));;
