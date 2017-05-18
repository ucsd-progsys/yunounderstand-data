
let listReverse l =
  let rec reverseHelper acc = if [] then acc else reverseHelper (h :: acc) t in
  reverseHelper [] l;;
