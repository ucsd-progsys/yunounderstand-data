
let rec digitsOfIntHelper n =
  if n > 0 then (digitsOfIntHelper (n / 10)) @ [n mod 10] else [];;

let rec digitsOfInt n = if n == 0 then [0] else digitsOfIntHelper n;;

let digits n = digitsOfInt (abs n);;

let rec sumList xs = match xs with | [] -> 0 | x::s -> x + (sumList s);;

let rec additivePersistence n =
  if n = 0 then 0 else 1 + (additivePersistence sumList (digits n));;
