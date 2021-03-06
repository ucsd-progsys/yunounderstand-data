(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXX
*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXX*) 
let rec sumList xs = 
  match xs with
    | [] -> 0
    | (x::xs') -> x + sumList xs'



let _ = sumList [1; 2; 3; 4]
let _ = sumList [1; -2; 3; 5]
let _ = sumList [1; 3; 5; 7; 9; 11]



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let digitsOfInt n = 
  let explodeNum =
    if (n > 0)
    then (n mod 10) :: (digitsOfInt (n/10))
    else []
  in listReverse explodeNum n

let _ = digitsOfInt 3124
let _ = digitsOfInt 352663
let _ = digitsOfInt (-5)
let _ = digitsOfInt (900000)
let _ = digitsOfInt 0


(*XXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXX
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

let rec additivePersistence n =
  match digitsOfInt n with
    | [x] -> 0
    | _ -> 1 + (additivePersistence (sumList (digitsOfInt n))) 


let _ = additivePersistence 9876
let _ = additivePersistence 123
let _ = additivePersistence 1
let _ = additivePersistence 987654322

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

let rec digitalRoot n = 
  if(sumList (digitsOfInt n) > 9)
  then digitalRoot (sumList (digitsOfInt n))
  else sumList (digitsOfInt n)

let _ = digitalRoot 9876
let _ = digitalRoot 1234
let _ = digitalRoot 12345
let _ = digitalRoot 001
let _ = digitalRoot 10000


let rec listReverse l = 
  match l with
    | [] -> []
    | (x::l') -> (listReverse l') @ [x]

let _ = listReverse [1; 2; 3; 4]
let _ = listReverse ["a"; "b"; "c"; "d"]


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)
let explode s = 
  let rec go i = 
    if i >= String.length s 
    then [] 
    else (s.[i]) :: (go (i+1)) 
  in
    go 0

let palindrome w = 
  let rec compareLists l1 l2 = 
    match (l1, l2) with
      | ([],[]) -> true;
      | (_::_, []) -> false (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
      | ([], _::_) -> false
      | (h1::l1',h2::l2') -> if h1 = h2 then (compareLists l1' l2') else false 
  in 
    compareLists (explode w) (listReverse (explode w))


let _ = palindrome "malayalam"
let _ = palindrome "myxomatosis"
let _ = palindrome "racecar"
let _ = palindrome "dood"
let _ = palindrome "james"





















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
