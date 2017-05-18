type 'a set = Set of 'a list

let empty = Set []

let member x s = match s with
  | Set [] -> false
  | Set h::tl -> if(h = x) then true else member x (Set tl)
