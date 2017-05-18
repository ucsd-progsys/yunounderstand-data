
let rec sepConcat sep sl =
  match sl with
  | [] -> ""
  | h::t ->
      let f a x = a ^ x in
      let base = "" in let l = h ^ (sep :: t) in List.fold_left f base l;;