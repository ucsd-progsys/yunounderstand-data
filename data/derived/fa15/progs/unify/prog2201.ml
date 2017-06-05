
let rec wwhile (f,b) =
  let res = f b in
  match res with | (x,y) when y = true -> wwhile (f, x) | (x,y) -> x;;

let collatz n =
  match n with | 1 -> 1 | _ when (n mod 2) = 0 -> n / 2 | _ -> (3 * n) + 1;;

let fixpoint (f,b) =
  let gs x =
    let rec go r = if (f r) = r then r else if (f r) <> r then go (f r) in
    ((go x), ((f x) = x)) in
  wwhile (gs, b);;

let _ = fixpoint (collatz, 1);;
