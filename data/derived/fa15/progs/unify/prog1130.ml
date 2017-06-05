
let rec wwhile (f,b) =
  let z = f b in
  match z with | (x,y) -> if y = false then x else wwhile (f, x);;

let fixpoint (f,b) =
  let newFunc = ((f b), ((b = (f b)) = false)) in wwhile (newFunc, b);;
