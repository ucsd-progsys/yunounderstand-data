
let rec reverse s =
  let s2 = String.length s in
  for i = 0 to (s2 - 1) / 2 do
    (let c = s.[i] in s.[i] <- s.[(s2 - i) - 1]; s.[(s2 - i) - 1] <- c)
  done;
  int_of_string s;;

let rec digitsOfInt n z =
  if n < 0
  then []
  else
    if z = 0
    then (let n2 = reverse (string_of_int n)
          and z = 1 in digitsOfInt n2)
    else
      (let x = n / 10
       and y = n mod 10 in
       if (x = 0) && (y = 0) then [] else y :: (digitsOfInt x));;
