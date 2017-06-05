
let rec digitsOfInt n =
  if n < 0
  then []
  else
    (let a = n mod 10 in
     let b = n / 10 in
     match b with
     | 0 -> if a = 0 then [] else (digitsOfInt b) @ [a]
     | x -> (digitsOfInt b) @ [a]);;

let digits n = digitsOfInt (abs n);;

let rec sumList xs = match xs with | [] -> 0 | h::t -> h + (sumList t);;

let oneRoot n =
  let x = digits n in
  let y = sumList x in
  let a = y mod 10 in
  let b = y / 10 in match b with | 0 -> if a = 0 then 0 else y | z -> y;;

let rec addHelp (n,m) =
  let x = oneRoot n in match x with | 0 -> m + 1 | z -> addHelp (x, (m + 1));;

let rec additivePersistence n = let (a,b) = addHelp (n, 0) in b;;
