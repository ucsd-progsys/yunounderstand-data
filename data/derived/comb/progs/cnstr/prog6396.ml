
let rec listReverse l =
  match l with | [] -> None | front::back -> [listReverse back; front];;
