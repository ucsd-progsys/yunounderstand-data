(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXX
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
  let f a x = (x*x) + a in
  let base = 0 in
    List.fold_left f base xs

let _ = sqsum []
let _ = sqsum [1;2;3;4]
let _ = sqsum [(-1); (-2); (-3); (-4)]
let _ = sqsum [1;2]
let _ = sqsum [1]
let _ = sqsum [2]
let _ = sqsum [1; (-1)]
let _ = sqsum [0]
let _ = sqsum [(-1)]

let incr x = x+1;;
let decr x = x-1;;

let foo = List.map [incr] 9

let pipe fs = 
  let f a x = fun f' -> (x (a f')) in
  let base =  fun base' -> base' in
    List.fold_left f base fs


let _ = pipe [] 3

let _ = pipe [(fun x -> x+x); (fun x -> x + 3)] 3

let _ = pipe [(fun x -> x + 3);(fun x-> x + x)] 3

let _ = pipe [(fun x -> x * x); (fun x -> x + 1)] 3

let _ = pipe [(fun x -> x); (fun x -> x*x)] (-1)


let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l



let _ = sepConcat ", " ["foo";"bar";"baz"]
let _ = sepConcat "---" []
let _ = sepConcat "" ["a";"b";"c";"d";"e"]
let _ = sepConcat "X" ["hello"]
let _ = sepConcat "X" ["hello";"philip";"ngo"]



let stringOfList f l = "[" ^ sepConcat "; " (List.map f l)^ "]"


let _ = stringOfList string_of_int [1;2;3;4;5;6];;
let _ = stringOfList (fun x -> x) ["foo"];;
let _ = stringOfList (stringOfList string_of_int) [[1;2;3];[4;5];[6];[]];;
let _ = stringOfList string_of_int [1];;
let _ = stringOfList string_of_int [];;



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

let rec clone x n = if n <= 0 then []
  else x::(clone x (n-1))


let _ = clone 3 5;;
let _ = clone "foo" 2;; 
let _ = clone clone (-3);;
let _ = clone "hello" (0);;


let padZero l1 l2 = if (List.length l1) > (List.length l2)
  then (l1, (clone 0 (List.length l1 - List.length l2)) @ l2)
  else ((clone 0 (List.length l2 - List.length l1)) @ l1, l2)


let _ = padZero [9;9] [1;0;0;2]
let _ = padZero [1;0;0;2] [9;9] 
let _ = padZero [1] [2]
let _ = padZero [] []
let _ = padZero [1] []
let _ = padZero [] [1;2;3]
let _ = padZero [1;3] [1]


let rec removeZero l = match l with
  | []    -> []
  | h::t  -> if h=0 then removeZero t
      else h::t


let _ = removeZero [0;0;0;1;0;0;2]
let _ = removeZero [9;9]
let _ = removeZero [0;0;0;0]
let _ = removeZero []
let _ = removeZero [0;1]


let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x =
      match a with
          (c,sum) ->
            match x with
                (d1,d2) -> let c' = (c + d1 + d2)/10 in
                  let d' = (c + d1 + d2) mod 10 in (c', d'::sum) in
    let base = (0, []) in
    let args = List.combine (List.rev (0::l1)) (List.rev (0::l2)) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))


let _ = bigAdd [9;9] [1;0;0;2];;
let _ = bigAdd [9;9;9;9] [9;9;9];; 
let _ = bigAdd [] [];;
let _ = bigAdd [1] [];;
let _ = bigAdd [1] [1];;
let _ = bigAdd [9] [9];;


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let rec mulByDigit i l = 
  if i = 1 then l
  else if i = 0 then []
  else if i < 0 then match (bigAdd (bigAdd l l) (mulByDigit ( (i*(-1)) -2) l)) with
    | h::t -> (h*(-1))::t
    | _ -> []
  else bigAdd (bigAdd l l) (mulByDigit (i-2) l)


let _ = mulByDigit 9 [9;9;9;9]
let _ = mulByDigit (-9) [9;9;9;9]
let _ = mulByDigit (0) [9;9;9;9]
let _ = mulByDigit 9 []
let _ = mulByDigit 9 [1]
let _ = mulByDigit (-9) []


let bigMul l1 l2 = 
  let f a x = match a with
      (c,sum) -> (c + 1, bigAdd ((mulByDigit x l1) @ clone 0 c) sum) in
  let base = (0, []) in
  let args = List.rev l2 in
  let (_, res) = List.fold_left f base args in
    res


let _ = bigMul [9;9;9;9] [9;9;9;9]
let _ = bigMul [9;9;9;9;9] [9;9;9;9;9] 
let _ = bigMul [] []
let _ = bigMul [1;2] []
let _ = bigMul [1] [0]
let _ = bigMul [0] []
let _ = bigMul [2] []
let _ = bigMul [1] [2]
let _ = bigMul [1;2] [1;1]
let _ = bigMul [1;2] [1;0]
let _ = bigMul [1;0] [1;2]
let _ = bigMul [1;2] [0;1]
let _ = bigMul [0;2] [4;0]


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

