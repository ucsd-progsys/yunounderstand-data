
let rec wwhile (f,b) =
  let x = f b in
  let h::t = x in
  let r::l = t in match t with | false  -> h | true  -> wwhile (f, h);;
