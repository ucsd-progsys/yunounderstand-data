
let rec wwhile (f,b) = let (b',c') = f b in if c' then wwhile (f, b') else b';;

let collatz n =
  match n with | 1 -> 1 | _ when (n mod 2) = 0 -> n / 2 | _ -> (3 * n) + 1;;

let fixpoint (f,b) = wwhile (f, b);;

let _ = fixpoint (collatz, 1);;
