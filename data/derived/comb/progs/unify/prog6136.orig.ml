type 'a set = Set of 'a list

let empty = Set []

let rec member x s = match s with
  | Set [] -> false
  | Set l -> let (h::tl) = l in if(h = x) then true else member x (Set tl)


let add x s = match s with
  | Set l -> Set (x::l)

let union s1 s2 = match s2 with
  | Set x2s -> List.fold_left (fun s x -> add x s) s1 x2s

let del x s = match s with
  | Set l -> let meh = Set(List.filter (fun z -> z=x) l) in 
        Set(meh)

let test = Set [1;2;3;4;5]
let hmm = Set [12;23;14;56]


let _ = del 12 hmm



let _ = member 20 test

let _ = add test hmm

let s0 = empty

let _, _ = (List.mem 1 s0, List.mem 2 s0)
