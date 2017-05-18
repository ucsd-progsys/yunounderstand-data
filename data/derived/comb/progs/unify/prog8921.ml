
let rec wwhile (f,b) =
  let (value,result) = f b in if result then wwhile (f, value) else value;;

let fixpoint (f,b) =
  wwhile
    ((let helper b' = let result = f b' in (result, ((not result) = b')) in
      helper), b);;

let _ =
  let g x = truncate (1e6 *. (cos (1e-6 *. (float x)))) in fixpoint (g, 0);;
