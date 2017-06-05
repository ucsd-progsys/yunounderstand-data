
let rec digitsOfInt n =
  if n <= 0 then [] else (digitsOfInt (n / 10)) @ [n mod 10];;

let digits n = digitsOfInt (abs n);;

let rec listReverse l = match l with | h::t -> [h] @ (listReverse [digits t]);;
