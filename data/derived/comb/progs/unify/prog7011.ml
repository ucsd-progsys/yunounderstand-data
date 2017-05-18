
let collatz n =
  match n with | 1 -> 1 | _ when (n mod 2) = 0 -> n / 2 | _ -> (3 * n) + 1;;

let isFPoint s = (collatz s) = s;;

let _ = (isFPoint 1) = 1;;
