
let rec wwhile (f,b) =
  match f b with | (b',c') -> if c' then wwhile (f, b') else b';;

let fixpoint (f,b) =
  wwhile
    ((match b with | b' -> if b = b' then (b', false) else (b', true)), b);;
