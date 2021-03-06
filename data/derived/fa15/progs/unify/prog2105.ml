
let rec wwhile (f,b) =
  let res = f b in
  match res with | (x,y) when y = true -> wwhile (f, x) | (x,y) -> x;;

let fixpoint (f,b) =
  let fx x = if x = 0 then 0 else if x > 1 then x - 1 else x + 1 in
  wwhile ((fx b), b);;
