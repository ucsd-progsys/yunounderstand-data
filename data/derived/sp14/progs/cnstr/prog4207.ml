
let rec digitsOfInt n = if n > 10 then (n mod 10) :: (digitsOfInt (n / 10));;
