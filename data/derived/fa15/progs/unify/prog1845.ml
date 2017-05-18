
let rec filter (l,a) =
  match l with
  | [] -> []
  | hd::tl -> if hd = a then filter (tl, a) else hd :: (filter (tl, a));;

let removeDuplicates l =
  let rec helper (seen,rest) =
    match rest with
    | [] -> seen
    | h::t ->
        let seen' = h :: seen in
        let rest' = filter t h in helper (seen', rest') in
  List.rev (helper ([], l));;
