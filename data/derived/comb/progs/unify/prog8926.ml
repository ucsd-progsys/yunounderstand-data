
let rec wwhile (f,b) =
  let (value,result) = f b in if result then wwhile (f, value) else value;;

let collatz n =
  match n with | 1 -> 1 | _ when (n mod 2) = 0 -> n / 2 | _ -> (3 * n) + 1;;

let fixpoint (f,b) =
  wwhile
    ((let helper b' = let result = f b' in (result, ((not result) = b')) in
      helper), b);;

let _ = fixpoint (collatz, 9001);;
