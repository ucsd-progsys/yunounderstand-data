let rec split l =
  let base = (0,[],[]) in
  let fold_fn(i,l1,l2) elmt =
    if (i < l/2) then (i+1,[elmt]@l1,l2)
    else (i,l1,[elmt]@l2) in
  let (_,l1,l2) = List.fold_left fold_fn base l in (l1,l2)


let rec merge l1 l2 = 
  match (l1,l2) with
    | ([],l) -> l
    | (l,[]) -> l
    | (h1::tl1,h2::tl2) -> if (h1 < h2) then h1::(merge tl1 l2)
        else h2::(merge l1 tl2)

let _ = merge [2;4;6;8] [1;3;5];;

let _ = merge [2;10;20] [1;2;3;4;5;8;10;12]
