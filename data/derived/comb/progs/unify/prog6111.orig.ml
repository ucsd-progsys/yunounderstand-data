type 'a set = Set of 'a list

let empty = Set [false;false]

let rec member x s = match s with
  | Set [] -> false
  | Set l -> let (h::tl) = l in if(h = x) then true else member x (Set tl)


let add x s = match s with
  | Set l -> match x with Set z -> z@l

let test = Set [1;2;3;4;5]
let hmm = Set [12;23;14;56]

let _ = member 20 test

let _ = add test hmm

let s0 = empty


           (List.mem 1 s0, List.mem 2 s0)
