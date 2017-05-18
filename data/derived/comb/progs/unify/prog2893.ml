
let rec wwhile (f,b) = match b with | false  -> b | true  -> wwhile (f, b);;

let _ = let f x = let xx = (x * x) * x in (xx, (xx < 100)) in wwhile (f, 2);;