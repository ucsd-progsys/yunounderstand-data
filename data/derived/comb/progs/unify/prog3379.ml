
let rec listReverse l =
  match l with | [] -> [] | h1 -> [h1] | h::t -> h :: (listReverse t);;