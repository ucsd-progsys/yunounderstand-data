(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXX
XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)
let rec assoc (d,k,l) = 
  match l with
    |[]->d
    |(name, age)::l'->if k=name then age
        else assoc(d,k,l')

let _ = assoc (-1,"william",[("ranjit",85);("william",23);("moose",44)]);;    
let _ = assoc (-1,"bob",[("ranjit",85);("william",23);("moose",44)]);;
let _ = assoc([1,1],"a",[("ca",[1,2]);("aa",[2,2])])
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

let rec filter l h=
  match l with
      []->[]
    |hd::tl -> if hd=h then (filter tl h)
        else hd::(filter tl h)
;;

let removeDuplicates l = 
  let rec helper (seen,rest) = 
    match rest with 
        [] -> seen
      | h::t -> 
          let seen' = h::seen in
          let rest' = (filter t h) in 
            helper (seen',rest') 
  in
    List.rev (helper ([],l))


let _ = removeDuplicates [1;6;2;4;12;2;13;6;9];;

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)
let rec wwhile (f,b) = 
  let (b',c')=f b in 
    if c'=true then wwhile(f,b')
    else b'
;;


let f x = let xx = x*x*x in (xx, xx < 100) in
  wwhile (f, 2);;


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

let fixpoint (f,b) = let f1 f2 x= if (f2 x)=x then (x,false) else (f2 x,true) in wwhile ((f1 f),b)


let g x = truncate (1e6 *. cos (1e-6 *. float x)) in fixpoint (g, 0);; 

let collatz n = match n with 1 -> 1 | _ when n mod 2 = 0 -> n/2 | _ -> 3*n + 1;;

let _ =collatz 2;;

let _ = fixpoint (collatz, 1) ;;
let _ = fixpoint (collatz, 3) ;;
let _ = fixpoint (collatz, 48) ;;
let _ = fixpoint (collatz, 107) ;;
let _ = fixpoint (collatz, 9001) ;;

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*) 

type expr = 
      VarX
    | VarY
    | Sine     of expr
    | Cosine   of expr
    | Average  of expr * expr
    | Times    of expr * expr
    | Thresh   of expr * expr * expr * expr	
    | Mean3    of expr * expr * expr 
    | Square   of expr
    (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXX*)
let rec exprToString e = 
  match e with
    |Thresh(a,b,c,d) ->"("^exprToString a^"<"^exprToString b^"?"^exprToString c^":"^(exprToString d)^")"
    |VarX		 ->"x"
    |VarY		 ->"y"
    |Sine(x)	 ->"sin(pi*"^(exprToString x)^")"
    |Cosine(x)       ->"cos(pi*"^(exprToString x)^")"
    |Average(e1,e2)  -> "("^(exprToString e1) ^"+"^(exprToString e2)^")/2"
    |Times(e1,e2)    ->(exprToString e1)^"*"^(exprToString e2)
    |Mean3(e1,e2,e3)     ->"("^(exprToString e1)^"+"^(exprToString e2)^"+"^(exprToString e3)^")/2"
    |Square(e)	 ->"("^(exprToString e)^")^2"
;;



let sampleExpr1 = Thresh(VarX,VarY,VarX,(Times(Sine(Mean(VarX,VarX,VarY)),Cosine(Average(Square(VarX),VarY)))))

let _ = exprToString sampleExpr1 


(*XXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildMean3(e1,e2,e3)	   = Mean3(e1,e2,e3)
let buildSquare(e)		   = Square(e)

let pi = 4.0 *. atan 1.0

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

let rec eval (e,x,y) = 
  match e with 
    |VarX ->x
    |VarY ->y
    |Sine(a)->sin(pi*.eval(a,x,y))
    |Cosine(a)->cos(pi*.eval(a,x,y))
    |Average(a,b)->(eval(a,x,y)+.eval(b,x,y))/.2.0
    |Times(a,b)->eval(a,x,y)*.eval(b,x,y)
    |Thresh(a,b,a_less,b_less)->let x1=eval(a,x,y) in let x2=eval(b,x,y)  in  if(x1<x2) then eval(a_less,x,y) else eval( b_less,x,y)
    |Mean3(a,b,c)->(eval(a,x,y)+.eval(b,x,y)+.eval(c,x,y))/.3.0
    |Square(a)->let tempp=eval(a,x,y) in tempp*.tempp
;;

let _ = eval (Sine(Average(VarX,VarY)),0.0,-0.5);;
let _ = eval (Sine(Average(VarX,VarY)),0.3,0.3);;
let _ =eval(sampleExpr1,0.9,0.5)

let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
    assert (-1.0 <= rv && rv <= 1.0);
    rv

let sampleExpr =
  buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
                                                              buildX()),buildTimes(buildCosine (buildCosine (buildAverage
                                                                                                               (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
                                                                                   buildCosine (buildTimes (buildSine (buildCosine
                                                                                                                         (buildY())),buildAverage (buildSine (buildX()), buildTimes
                                                                                                                                                                           (buildX(),buildX()))))))),buildY())))
let _=eval(sampleExpr,0.5,0.2)
let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXX
*)

let rec build (rand, depth) = 
  match depth with
    |0->(match rand(0,1) with
          |0->VarX
          |1->VarY)
    |n-> match rand(0,8) with
      |0->buildSine(build(rand,depth-1))
      |1->buildCosine(build(rand,depth-1))
      |2->buildAverage(build(rand,depth-1),build(rand,depth-1))
      |3->buildTimes(build(rand,depth-1),build(rand,depth-1))
      |4->buildThresh(build(rand,depth-1),build(rand,depth-1),build(rand,depth-1),build(rand,depth-1))
      |5->buildCube(build(rand,depth-1))
      |6->buildSquare(build(rand,depth-1))
;;

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

let g1 () = (5,91,13)
let g2 () = (3,66,12)  
let g3 () = (10,88,9) 

let c1 () = (8,90,10)
let c2 () = (11,100,11)
let c3 () = (2,222,12)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXX

XXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXX*)
*)

let makeRand (seed1, seed2) = 
  let seed = (Array.of_list [seed1;seed2]) in
  let s = Random.State.make seed in
    (fun (x,y) -> (x + (Random.State.int s (y-x))))


let rec rseq g r n =
  if n <= 0 then [] else (g r)::(rseq g r (n-1))

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*X
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let toReal (i,n) = (float_of_int i) /. (float_of_int n)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let toIntensity z = int_of_float (127.5 +. (127.5 *. z))


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

let rec ffor (low,high,f) = 
  if low > high then () else 
    let _ = f low in 
      ffor (low+1,high,f)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

let emitGrayscale (f,n,name) =
  (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let fname  = ("art_g_"^name) in
  let chan = open_out (fname^".pgm") in
  (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let n2p1 = n*2+1 in   
  let _ = output_string chan (Format.sprintf "P5 %d %d 255\n" n2p1 n2p1) in
  let _ = 
    ffor (-n, n, 
          fun ix ->
            ffor (-n, n, 
                  fun iy ->
                    (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                    let x = toReal(ix,n) in
                    let y = toReal(iy,n) in
                    (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                    let z = f (x,y) in
                    (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                    let iz = toIntensity(z) in
                      (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                      output_char chan (char_of_int iz))) in 
    close_out chan;
    ignore(Sys.command ("convert "^fname^".pgm "^fname^".jpg"));
    ignore(Sys.command ("rm "^fname^".pgm"))

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

let doRandomGray (depth,seed1,seed2) =
  (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let g = makeRand(seed1,seed2) in
  (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let e = build (g,depth) in
  let _ = print_string (exprToString e) in
  let f = eval_fn e in
  (*XXXXXXXXXXXXXXXXXX*)
  let n = 150 in
  (*XXXXXXXXXXXXXXXXXX*)
  let name = Format.sprintf "%d_%d_%d" depth seed1 seed2 in
    emitGrayscale (f,n,name)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

*)


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXX
*)
let emitColor (f1,f2,f3,n,name) =
  (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let fname  = ("art_c_"^name) in
  let chan = open_out (fname^".ppm") in
  (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let n2p1 = n*2+1 in   
  let _ = output_string chan (Format.sprintf "P6 %d %d 255\n" n2p1 n2p1) in
  let _ = 
    ffor (-n, n, 
          fun ix ->
            ffor (-n, n, 
                  fun iy ->
                    (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                    let x = toReal(ix,n) in
                    let y = toReal(iy,n) in
                    (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                    let z1 = f1 (x,y) in
                    let z2 = f2 (x,y) in
                    let z3 = f3 (x,y) in

                    (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                    let iz1 = toIntensity(z1) in
                    let iz2 = toIntensity(z2) in
                    let iz3 = toIntensity(z3) in

                      (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                      output_char chan (char_of_int iz1);
                      output_char chan (char_of_int iz2);
                      output_char chan (char_of_int iz3);
                 )) in  
    close_out chan;
    ignore(Sys.command ("convert "^fname^".ppm  "^fname^".jpg"));
    ignore(Sys.command ("rm "^fname^".ppm")) 

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXX
*)
let doRandomColor (depth,seed1,seed2) =
  (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let g = makeRand (seed1,seed2) in
  (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let e1 = build (g, depth) in
  let e2 = build (g, depth) in
  let e3 = build (g, depth) in

  let _ = Format.printf "red   = %s \n" (exprToString e1) in
  let _ = Format.printf "green = %s \n" (exprToString e2) in
  let _ = Format.printf "blue  = %s \n" (exprToString e3) in

  let f1 = eval_fn e1 in
  let f2 = eval_fn e2 in
  let f3 = eval_fn e3 in

  (*XXXXXXXXXXXXXXXXXX*)
  let n = 150 in
  (*XXXXXXXXXXXXXXXXXX*)
  let name = Format.sprintf "%d_%d_%d" depth seed1 seed2 in
    emitColor (f1,f2,f3,n,name)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

type test = unit -> string

let key = "" (*XXXXXXXX*)
let prefix130 = "130" (*XXXXXXXX*)
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

let mkTest f x y name = runTest ((f, x, y), 1, name)

let badTest () = "WARNING: Your tests are not valid!!\n"

let scoreMsg () = 
  Format.sprintf "Results: Score/Max = %d / %d \n" !score !max 

let sampleTests =
  [
    (fun () -> mkTest
                 assoc
                 (-1, "william", [("ranjit",85);("william",23);("moose",44)])
                 23
                 "sample: assoc 1"
    );
    (fun () -> mkTest 
                 assoc
                 (-1, "bob", [("ranjit",85);("william",23);("moose",44)])
                 (-1)
                 "sample: assoc 2"
    ); 
    (fun () -> mkTest 
                 removeDuplicates
                 [1;6;2;4;12;2;13;6;9]
                 [1;6;2;4;12;13;9]
                 "sample: removeDuplicates 2"
    );
    (fun () -> mkTest 
                 removeDuplicates
                 [1;1;1]
                 [1]
                 "sample: removeDuplicates 2"
    );

    (fun () -> mkTest 
                 wwhile 
                 ((fun x -> let xx = x*x*x in (xx, xx < 100)), 2) 
                 512 
                 "sample: wwhile 1"
    ); 
    (fun () -> mkTest 
                 fixpoint
                 ((fun x -> truncate (1e6 *. cos (1e-6 *. float x))), 0)
                 739085
                 "sample: fixpoint 1"
    ); 

    (fun () -> mkTest 
                 emitGrayscale
                 (eval_fn sampleExpr, 150,"sample")
                 ()
                 "sample: eval_fn 1: manual"
    ); 
    (fun () -> mkTest 
                 emitGrayscale
                 (eval_fn sampleExpr2, 150,"sample2")
                 ()
                 "sample: eval_fn 2: manual"
    );

    (fun () -> mkTest 
                 (fun () -> doRandomGray (g1 ()))
                 ()
                 ()
                 "sample: gray 1 : manual"
    );
    (fun () -> mkTest 
                 (fun () -> doRandomGray (g2 ()))
                 ()
                 ()
                 "sample: gray 2 : manual"
    );
    (fun () -> mkTest 
                 (fun () -> doRandomGray (g3 ()))
                 ()
                 ()
                 "sample: gray 3 : manual"
    );

    (fun () -> mkTest 
                 (fun () -> doRandomColor (c1 ()))
                 ()
                 ()
                 "sample: color 1 : manual"
    );
    (fun () -> mkTest 
                 (fun () -> doRandomColor (c2 ()))
                 ()
                 ()
                 "sample: color 2 : manual"
    );
    (fun () -> mkTest 
                 (fun () -> doRandomColor (c3 ()))
                 ()
                 ()
                 "sample: color 3 : manual"
    )] 

let doTest f = 
  try f () with ex -> 
    Format.sprintf "WARNING: INVALID TEST THROWS EXCEPTION!!: %s \n\n"
      (Printexc.to_string ex)

let _ =
  let report = List.map doTest sampleTests                in
  let _      = List.iter print130 (report@([scoreMsg()])) in
  let _      = print130 ("Compiled\n")                    in
    (!score, !max)

