(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXX
*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXX
XXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXX
XXXXXXXXXXXXXXX
XXXXXXXXXXXXXX
XXXXXXXXXXXXXX
XXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXX
*)



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


let sqsum xs = 
  let f a x =  a + x*x in
  let base = 0 in
    List.fold_left f base xs


let _ = sqsum []
let _ = sqsum [1;2;3;4]
let _ = sqsum [(-1); (-2); (-3); (-4)]



let pipe fs = 
  let f a x =  x a  in
  let base = failwith "to be implemented" in
    List.fold_left f base fs



let _ = pipe [] 3

let _ = pipe [(fun x -> x+x); (fun x -> x + 3)] 3

let _ = pipe [(fun x -> x + 3);(fun x-> x + x)] 3



let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x  in
      let base = h in
      let l = t in
        List.fold_left f base l




let _ = sepConcat ", " ["foo";"bar";"baz"]
let _ = sepConcat "---" []
let _ = sepConcat "" ["a";"b";"c";"d";"e"]
let _ = sepConcat "X" ["hello"]
let _ = sepConcat "a" []
let _ = sepConcat "a" ["b"]
let _ = sepConcat "" []
let _ = sepConcat "" ["bc"]
let _ = sepConcat "z" [""]



let stringOfList f l = "[" ^ sepConcat ";" (List.map f l) ^ "]"


let _ = stringOfList string_of_int [1;2;3;4;5;6];;
let _ = stringOfList (fun x -> x) ["foo"];;
let _ = stringOfList (stringOfList string_of_int) [[1;2;3];[4;5];[6];[]];;




(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

let rec clone x n = 
  let rec helper a x n =
    if n <= 0 then a
    else 
      let a' = x::a in
        helper a' x (n-1)
  in helper [] x n;;


let _ = clone 3 5;;
let _ = clone "foo" 2;; 
let _ = clone clone (-3);;


let padZero l1 l2 = 
  let length1 = List.length l1 in
  let length2 = List.length l2 in
    if length1 > length2 then 
      (l1, List.append (clone 0 (length1 - length2)) l2)
    else if length2 > length1 then 
      (List.append (clone 0 (length2 - length1)) l1, l2)
    else (l1,l2)

let _ = padZero [9;9] [1;0;0;2]
let _ = padZero [1;0;0;2] [9;9]
let _ = padZero [1;0;0;2] []
let _ = padZero [77] [9;9;4;5;6;7;8;9;0]
let _ = padZero [] []
let _ = padZero [] [9]
let _ = padZero [0;0] [1]


let rec removeZero l = match l with
  | [] -> []
  | h::t -> if h = 0 then removeZero t else l


let _ = removeZero [0;0;0;1;0;0;2]
let _ = removeZero [9;9]
let _ = removeZero [0;0;0;0]

let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = match x with
        (x1, x2) -> 
          match a with
              (o , z) -> if o + x1 + x2 > 9 
                then (1, ((o + x1 + x2) mod 10)::z )
                else (0, (o + x1 + x2)::z)
    in
    let base = (0, []) in
    let args = List.append (List.combine (List.rev l1) (List.rev l2)) [(0,0)]
    (*X
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)
    in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))


let _ = bigAdd [9;9] [1;0;0;2];;
let _ = bigAdd [9;9;9;9] [9;9;9];; 
let _ = bigAdd [1;2;3] [];;
let _ = bigAdd [] [];;
let _ = bigAdd [] [3;4;5];;
let _ = bigAdd [1] [9;9;9];;
let _ = bigAdd [0;0;0] [9;9;9];;
let _ = bigAdd [0;0;0;0] [9;9;9];;
let _ = bigAdd [0] [];;
let _ = bigAdd [3] [];;
let _ = bigAdd [0] [0];;





let rec mulByDigit i l =
  let rec helper acc carry i l = match l with
    | [] -> (match acc with
              | [] -> []
              | h::t -> if h = 0 then t else acc)
    | h::t -> let x = (h*i + carry) in 
        let n = if x > 9 then x mod 10 else x in
        let carry' = if x > 9 then x/10 else 0 in
        let acc' = n::acc in
          helper acc' carry' i t
  in helper [] 0 i (List.rev (0::l))


let _ = mulByDigit 9 [9;9;9;9]
(*XXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXX*)
let _ = mulByDigit 0 [1;2;3]
let _ = mulByDigit 1 [1;2;3]
let _ = mulByDigit 10 [1;2;3]
(*XXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXX*)
let _ = mulByDigit 5 [0;0;0]
let _ = mulByDigit 9 []
(*XXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXX*)
let _ = mulByDigit 0 []


let bigMul l1 l2 = 
  let f a x =  match a with 
    | (_, z) -> (0, bigAdd z x) in
  let base = (0, []) in
  let args = 
    let rec digitProducts acc place l1 l2 = match l1 with
      | [] -> acc
      | h::t -> 
          let placeHolders = 
            (let rec buildZeros a places = 
              if places = 0 then a else buildZeros (0::a) (places-1) in 
               buildZeros [] place) in 
          let a = 
            (List.append (mulByDigit h l2) placeHolders) in
            digitProducts (a::acc) (place+1) t l2 in
      digitProducts [] 0 (List.rev l1) l2 in
  let (_, res) = List.fold_left f base args in
    res;;


let _ = bigMul [9;9;9;9] [9;9;9;9]
let _ = bigMul [9;9;9;9;9] [9;9;9;9;9] 
let _ = bigMul [] []
let _ = bigMul [0] [0]
let _ = bigMul [] [0]
let _ = bigMul [0] []
let _ = bigMul [] [1;2;3]
let _ = bigMul [1;2;3] []
let _ = bigMul [0] [1;2;3]
let _ = bigMul [1;2;3] [0]
let _ = bigMul [1] [1;2;3]
let _ = bigMul [1;2;3] [1]
let _ = bigMul [1;2;3] [5]
let _ = bigMul [5] [1;2;3]
let _ = bigMul [1;2;3] [5;6]
let _ = bigMul [5;6] [1;2;3]
let _ = bigMul [1;0;0] [5]
let _ = bigMul [5;2;1] [0;0]
let _ = bigMul [1;2;3;3] [0;0;0]
let _ = bigMul [5] [5]





(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
























(*XXXXXXXXXXXXXXXXXXXXXXXXXX*)

let key = "" (*XXXXXXXX*)
let prefix130 = "130" (*XXXXXXXX*)
let print130 s = print_string (prefix130^">>"^s)

exception ErrorCode of string

type result = Pass | Fail | ErrorCode of string

let score = ref 0
let max = ref 0
let timeout = 300

let runWTimeout (f,arg,out,time) = 
  try if compare (f arg) out = 0 then Pass else Fail
  with e -> (print130 ("Uncaught Exception: "^(Printexc.to_string e)^"\n"); ErrorCode "exception") 

exception TestException
let testTest () =
  let testGood x = 1 in
  let testBad x = 0 in 
  let testException x = raise TestException in
  let rec testTimeout x = testTimeout x in
    runWTimeout(testGood,0,1,5) = Pass &&  
    runWTimeout(testBad,0,1,5) = Fail &&  
    runWTimeout(testException,0,1,5) = ErrorCode "exception" && 
    runWTimeout(testTimeout,0,1,5) = ErrorCode "timeout"


let runTest (f,arg,out,points,name) =
  let _ = max := !max + points in
  let outs = 
    match runWTimeout(f,arg,out,timeout) with 
        Pass -> (score := !score + points; "[pass]")
      | Fail -> "[fail]"
      | ErrorCode e -> "[error: "^e^"]"  in
    name^" "^outs^" ("^(string_of_int points)^")\n"

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
    _exp 0

let implode cs = 
  String.concat "" (List.map (String.make 1) cs)

let drop_paren s = 
  implode (List.filter (fun c -> not (List.mem c ['(';' ';')'])) (explode s))

let eq_real p (r1,r2) = 
  (r1 -. r2) < p || (r2 -. r1) < p

let wrap_curried_2 f (a,b) = f a b

let runAllTests () =
  let _ = (score := 0; max := 0) in
  let report = 
    [runTest (sqsum, [], 0, 1, "sqsum 1");
     runTest (sqsum, [1;2;3;4], 30, 1, "sqsum 2");
     runTest (sqsum, [-1;-2;-3;-4], 30, 1, "sqsum 3");

     runTest (wrap_curried_2 pipe, ([], 3), 3, 1, "pipe 1");
     runTest (wrap_curried_2 pipe, ([(fun x-> 2*x);(fun x -> x + 3)], 3), 9, 1, "pipe 2");
     runTest (wrap_curried_2 pipe, ([(fun x -> x + 3); (fun x-> 2*x)], 3), 12, 1, "pipe 3");

     runTest(wrap_curried_2 sepConcat, (", ",["foo";"bar";"baz"]), "foo, bar, baz", 1, "sepConcat 1");
     runTest(wrap_curried_2 sepConcat, ("---",[]), "", 1, "sepConcat 2");
     runTest(wrap_curried_2 sepConcat, ("",["a";"b";"c";"d";"e"]), "abcde", 1, "sepConcat 3");
     runTest(wrap_curried_2 sepConcat, ("X",["hello"]), "hello", 1, "sepConcat 4");

     runTest(wrap_curried_2 stringOfList, (string_of_int,[1;2;3;4;5;6]), "[1; 2; 3; 4; 5; 6]",1,"stringOfList 1");
     runTest(wrap_curried_2 stringOfList, ((fun x -> x),["foo"]), "[foo]",1,"stringOfList 2");
     runTest(wrap_curried_2 stringOfList, ((stringOfList string_of_int),[[1;2;3];[4;5];[6];[]]), "[[1; 2; 3]; [4; 5]; [6]; []]",1,"stringOfList 3");

     runTest(wrap_curried_2 clone, (3,5), [3;3;3;3;3],1,"clone 1");
     runTest(wrap_curried_2 clone, ("foo",2), ["foo";"foo"],1,"clone 2");
     runTest(wrap_curried_2 clone, (clone,-3), [],1,"clone 3");

     runTest(wrap_curried_2 padZero, ([9;9],[1;0;0;2]), ([0;0;9;9],[1;0;0;2]),1,"padzero 1");
     runTest(wrap_curried_2 padZero, ([1;0;0;2],[9;9]), ([1;0;0;2],[0;0;9;9]),1,"padzero 2");

     runTest(removeZero, [0;0;0;1;0;0;2], [1;0;0;2],1,"removeZero 1");
     runTest(removeZero, [9;9], [9;9],1,"removeZero 2");

     runTest(wrap_curried_2 bigAdd,  ([9;9],[1;0;0;2]), [1;1;0;1],1, "bigAdd 1");
     runTest(wrap_curried_2 bigAdd,  ([9;9;9;9],[9;9;9]), [1;0;9;9;8],1, "bigAdd 2");

     runTest(wrap_curried_2 mulByDigit,  (9,[9;9;9;9]), [8;9;9;9;1],1, "mulByDigit 1");

     runTest(wrap_curried_2 bigMul,  ([9;9;9;9],[9;9;9;9]), [9;9;9;8;0;0;0;1],1, "bigMul 1");
     runTest(wrap_curried_2 bigMul,  ([9;9;9;9;9],[9;9;9;9;9]), [9;9;9;9;8;0;0;0;0;1],1,"bigMul 2");
    ] in
  let s = Format.sprintf "Results: Score/Max = %d / %d \n" !score !max in
  let _ = List.iter print130 (report@([s])) in
    (!score,!max)

let _ = runAllTests ()

let _ = print130 ("Compiled"^key^"\n")


