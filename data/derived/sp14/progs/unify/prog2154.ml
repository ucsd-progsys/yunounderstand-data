
let rec digitsOfInt n = if n < 0 then [] else [(digitsOfInt (n / 10)) mod 10];;
