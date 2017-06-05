
let sepConcat sep sl =
  match sl with
  | [] -> ""
  | h::t ->
      let f a x = a ^ (sl ^ x) in
      let base = h in let l = t in List.fold_left f base l;;
