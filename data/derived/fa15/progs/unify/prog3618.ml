
let rec wwhile (f,b) =
  let (b',c') = f b in if c' = true then wwhile (f, b') else b';;

let fixpoint (f,b) = ((wwhile (fun x  -> ((wwhile (f, b)), (b = (f b))))), b);;
