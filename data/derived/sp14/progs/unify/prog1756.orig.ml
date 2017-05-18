(*XXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXX
*)



(*XXXXXXXXXXXXXXXXXXXXXXXXXXX*) 
let rec sumList xs = match xs with
  | []     -> 0 
  | hd::tl -> hd + sumList tl
;;

let _ = sumList [1; 2; 3; 4];;
let _ = sumList [1; -2; 3; 5];;
let _ = sumList [1; 3; 5; 7; 9; 11];;

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

let rec digithelper n l=
  let x = n/10 in
  let y = n mod 10 in
    if x <= 0
    then y :: l
    else digithelper x (y :: l)
;;

let digitsOfInt n =
  if n = 0 
  then []
  else 
    digithelper n []
;;

let _ = digitsOfInt 3124;;
let _ = digitsOfInt 352663;;
let _ = digitsOfInt 0;;
let _ = digitsOfInt 1;;
let _ = digitsOfInt -12;;



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

let digits n = digitsOfInt (abs n)


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let rec additivePersistence n = failwith "TBD"

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

*)


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXX*)
let rec digitalRoot n = failwith "TBD"

(*
XXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXX
XXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXX
XX
*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXXXX

*)


(*XXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let listReverse l = 
  let rec helperFun l z = match l with
    | [] -> z
    | hd :: tl -> helperFun tl (hd :: z)
  in
    helperFun l []
;;

let _ = listReverse [1; 2; 3; 4];;
let _ = listReverse ["a"; "b"; "c"; "d"];;


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)
let explode s = 
  let rec go i = 
    if i >= String.length s 
    then [] 
    else (s.[i]) :: (go (i+1)) 
  in
    go 0


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let palindrome w = 
  let z = explode w in
  let y = listReverse z in
    if z = y
    then true
    else false
;;

let _ = palindrome "malayalam";;
let _ = palindrome "myxomatosis";;


























(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

type test = unit -> string

let key        = ""     (*XXXXXXXX*)
let prefix130  = "130"  (*XXXXXXXX*)

let print130 s = print_string (prefix130^">>"^s)

exception ErrorCode of string

exception TestException

type result = Pass | Fail | ErrorCode of string

let score = ref 0
let max = ref 0
let timeout = 300

let runWTimeout (f,arg,out,time) = 
  try if compare (f arg) out = 0 then Pass else Fail
  with e -> (print130 ("Uncaught Exception: "^(Printexc.to_string e)); ErrorCode "exception") 

let testTest () =
  let testGood x = 1 in
  let testBad x = 0 in 
  let testException x = raise TestException in
  let rec testTimeout x = testTimeout x in
    runWTimeout(testGood,0,1,5) = Pass &&  
    runWTimeout(testBad,0,1,5) = Fail &&  
    runWTimeout(testException,0,1,5) = ErrorCode "exception" && 
    runWTimeout(testTimeout,0,1,5) = ErrorCode "timeout"

let runTest ((f,arg,out),points,name) =
  let _   = max := !max + points in
  let outs = 
    match runWTimeout(f,arg,out,timeout) with 
        Pass -> (score := !score + points; "[pass]")
      | Fail -> "[fail]"
      | ErrorCode e -> "[error: "^e^"]"  in
    name^" "^outs^" ("^(string_of_int points)^")\n"

let mkTest f x y name = runTest ((f, x, y), 1, name)

let badTest () = "WARNING: Your tests are not valid!!\n"

let scoreMsg () = 
  Printf.sprintf "Results: Score/Max = %d / %d \n" !score !max 

let doTest f = 
  try f () with ex -> 
    Printf.sprintf "WARNING: INVALID TEST THROWS EXCEPTION!!: %s \n\n"
      (Printexc.to_string ex)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

let sampleTests =
  [
    (fun () -> mkTest
                 sumList
                 [1;2;3;4]
                 10
                 "sample: sumList 1"
    );
    (fun () -> mkTest 
                 sumList 
                 [1;-2;3;5] 
                 7 
                 "sample: sumList 2"
    ); 
    (fun () -> mkTest 
                 sumList 
                 [1;3;5;7;9;11]
                 36 
                 "sample: sumList 3"
    ); 
    (fun () -> mkTest 
                 digitsOfInt 
                 3124 
                 [3;1;2;4] 
                 "sample: digitsOfInt 1"
    ); 
    (fun () -> mkTest 
                 digitsOfInt 
                 352663 
                 [3;5;2;6;6;3] 
                 "sample: digitsOfInt 2"
    ); 
    (fun () -> mkTest 
                 digits
                 31243
                 [3;1;2;4;3] 
                 "sample: digits 1"
    ); 
    (fun () -> mkTest 
                 digits
                 (-23422)
                 [2;3;4;2;2]
                 "sample: digits 2"
    ); 
    (fun () -> mkTest 
                 additivePersistence 
                 9876 
                 2 
                 "sample: additivePersistence1"
    ); 
    (fun () -> mkTest 
                 digitalRoot 
                 9876 
                 3 
                 "sample: digitalRoot"
    ); 
    (fun () -> mkTest 
                 listReverse
                 [1;2;3;4] 
                 [4;3;2;1]
                 "sample: reverse 1"
    ); 
    (fun () -> mkTest 
                 listReverse 
                 ["a";"b";"c";"d"]
                 ["d";"c";"b";"a"] 
                 "sample: rev 2"
    ); 
    (fun () -> mkTest 
                 palindrome 
                 "malayalam" 
                 true
                 "sample: palindrome 1"
    ); 
    (fun () -> mkTest 
                 palindrome 
                 "myxomatosis" 
                 false
                 "sample: palindrome 2"
    )] 

let _ =
  let report = List.map doTest (sampleTests) in
  let _ = List.iter print130 (report@([scoreMsg()])) in
  let _ = print130 ("Compiled\n") in
    (!score, !max)
