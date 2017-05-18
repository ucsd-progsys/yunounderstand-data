type 'a set = Set of 'a list

let empty = Set []

let rec member x s = match s with
  | Set [] -> false
  | Set l -> let (h::tl) = l in if(h = x) then true else member x (Set tl)


let add x s = match s with
  | Set l -> l@x

let test = Set [1;2;3;4;5]
let hmm = Set [12;23;14;56]

let _ = member 20 test

let _ = add test hmm
