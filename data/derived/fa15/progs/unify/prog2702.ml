
let rec wwhile (f,b) =
  let (b',c) = f b in if not c then b' else wwhile (f, b');;

let fixpoint (f,b) =
  wwhile ((let f' b = ((f b), (((f b) not) = b)) in f'), b);;
