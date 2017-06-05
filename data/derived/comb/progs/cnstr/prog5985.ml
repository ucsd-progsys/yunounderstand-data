
let rec wwhile (f,b) =
  let x = f b in match x with | h::t -> if t = true then wwhile (f, h) else h;;
