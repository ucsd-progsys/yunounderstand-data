
let rec wwhile (f,b) =
  let res = f b in
  match res with | (x,y) when y = true -> wwhile (f, x) | (x,y) -> x;;

let collatz n =
  match n with | 1 -> 1 | _ when (n mod 2) = 0 -> n / 2 | _ -> (3 * n) + 1;;

let fixpoint (f,b) =
  let gs x =
    let isFPoint s = (f s) = s in
    let rec go r =
      if (isFPoint r) = true
      then r
      else if (isFPoint r) = false then go (f r) in
    ((go x), (isFPoint x)) in
  wwhile (gs, b);;

let _ = fixpoint (collatz, 3);;
