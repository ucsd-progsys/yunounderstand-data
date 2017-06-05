
let rec wwhile (f,b) =
  let x = f b in
  match x with | h::t -> if t = false then h else wwhile (f, h);;
