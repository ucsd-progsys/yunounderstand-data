
let rec wwhile (f,b) =
  let (b',c') = f b in match c' with | false  -> b' | _ -> wwhile (f, b');;

let fixpoint (f,b) =
  wwhile ((fun (f,b)  -> if (f b) = b then (b, false) else ((f b), true)), b);;
