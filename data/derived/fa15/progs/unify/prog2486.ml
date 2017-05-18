
let rec mulByDigit i l =
  match l with
  | [] -> []
  | x::x'::x'' ->
      [(x * i) / 10] @ ([((x * i) mod 10) + x'] @ ((mulByDigit i x') :: x''));;
