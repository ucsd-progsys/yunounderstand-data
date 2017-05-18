
let rec wwhile (f,b) =
  let x = wwhile (f, b) in
  let h::t = x in match t with | false  -> h | true  -> wwhile (f, h);;
