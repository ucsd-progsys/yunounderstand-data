
let rec digitsOfInt n =
  if n < 0
  then []
  else
    (let a = n / 10 in
     let b = n mod 10 in
     let c = [a; b] in
     if a > 9 then match c with | x::xs -> (digitsOfInt x) :: xs);;
