
let removeDuplicates l =
  let rec helper (seen,rest) =
    match rest with
    | [] -> seen
    | h::t ->
        let seen' = List.mem h t in let rest' = t in helper (seen', rest') in
  List.rev (helper ([], l));;
