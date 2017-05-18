(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXX
XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

let rec assoc (d,k,l) = 
  match l wi
      | (ki, vi)::> 
        if ki = k then
          vssoc (d,k,tl)
      |


        eDuplnt list -> int  
                        * or more generally, removeDuplicates : 'a list -> 'a lit
                                                                           * (removeDuplicates l) is the list of elements of l with uplicates (second,
                                                                                                                                               * third ... occurrences) removed, and where the remaining lements 
                                                                                                                                                                                     * appear in the same order as in l.
                                                                                                                                                                                                                        * e.g. (removeDuplicate [1;6;2;4;12;2;13;6;9]) is [1;6;2;4;2;13;9]
                                                                                                                                                                                                                      *
                                                                                                                                                                                                                      *  ** your function "helper" should be tail recursive **
                                                                                                                                                                                                                                          * orthis problem only, you may use the library function List.em and List.rev
                                                                                                                                                                                                                                                                                                              *)

let removeDuplicates l = 
  let rec helper (seen,rest) = 
    match rest with 
        [] -> seen
      | h::t> 
        let seen' = 
          if (List.mem hn) then
            seen
          else
        in
        let res in 
          helperst') 
in
  List ([],l))



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)
let rec wwhile (f,b) = 
  let (b', c') = f b in
    if c'then
         wwhile (f,b')
else
  b'
;;



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let fixpoint (f,b) = wwhile ((let g h x = let x = h x in (xx, x != xx) in g f),b)


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

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXX




XXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

   let buildX()t buildY()                       = VarY
   let buildSine(e)                   = Sine
   let buildCosine(e)                 = Cosine(e)
   let buildAvee(e1,e2)            = Average(e1,e2)
   let buildTimes(e1,e2)           = Times(e1,e2)
   let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)


   let pi = 4.0 *. atan 1.0

   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

   let rec eval (e,x,y) = match e with
   | VarX -> x
   | VarY -> y
   | Sine(e') -> sin(pi *. eval(e', x, y))
   | Cosine(e') -> cos(pi *. eval(e', x, y))
   | Average(e1, e2) -> ((eval(e1, x, y) +. eval(e2, x, y)) /. 2.0)
   | Times(e1, e2) ->  eval(e1, x, y) *. eval(e2, x, y)
   | Thresh(e1, e2, e3,  -> 
   if (eval(e1, x, y) < eval(e2, x, y)) then 
   eval(e3, x, y) 
   else 
   eval(e4x, y)
   ;;


   l eval_fn e (y) = 
   let rv = eval (e,x,y) in
   assert (-0 <= rv && rv <= 1.0);
   rv


   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXX
X*)

   let rec build (rand, h) = 
   if depth <= 0 then
   let bin_rand = rand(0, 2) in
   if bin_rand = 0 then buildX()
   else buildY()   (*XXXXXXXXXXX*)
   else 
   let exp_rand = rand(0, 5) in
   let first_forced =ild(rand, depth - 1) in
   match exp_rand with
   | 0 -> buildSine(fi_forced)
   | 1 -> buildCosine(first_forced)2 -> buildAverage(first_forced, build(rand, depth - 1))
   | 3 buildTimes(first_forced, build(rand, depth - 1))
   | -> buildThresh(firorced, build(rand, depth - 1)ld(rand, depth - 1), build(ranth - 1))
   ;;

   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

   (*XXXXXXXXXXXXXXXXXXXXXXXX*) 

   type expr = 
   VarX
   | VarY
   | Sine    
   | Cosine   of expr
   | Average  of expr * expr
   | Times    o expr * expr * expr * expr	
   | Acossin  of expr * expr
   | Crazy    of expr * expr * expr
   ;;


   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
X*)
   let rec exprToString e = 
   match e with
   | VarX			 -> "x"
   | VarY			 -> "y"
   | Sine(e')		 -> "sin(pi*" ^ exprToString(e') ^ ")"
   | Cosine(e')		 -> "cos(pi*" ^ exprToString(e') ^ ")"
   | Average(e1, e2)	(" ^ ToStrin) ^ "+" ^ exprToStre2) ^ ")/2)"
   | Time, e2)		 ->  exprToString(e "*" ^ exprToString(e2)
   | sh(e1, e2, e3, e4) -> "(" ^ exprToString(^ "<" ^ exprToString(e2) ^" ^ exprToString(e3) ^ ":" ^ exprToString(e4) ^ ")"
   | Acossin(e1, e2)	 "(acos(" ^ exprToString(e1) ^ ")*asin(" ^ 
   exprToString(e2) ^ ")*2/(pi*pi))"
   | Crazy1, e2, e3)   > 
   let s1 = exprTing(e1) in
   let s2xprToString(e2) in
   let s3 = exprToString(e3) in 
   s1>" ^ s2 ^ "?((" ^
   s1 ^ "+" ^ s2 ^ "+" ^s3 ^ ")/3:(" ^^ ">" ^ 
   s3 ^ "?(" ^ s1 ^ "*" ^ s2 ^ "+" ^ s1 ^ ")/2:(-" ^ s3 ^ ")"
   ;;




   (*XXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

   let buildX()                      arX
   let buildY()                       = VarY
   let buildSine(e)   let buildCosine(e)                osine(e)
   let buildAverage(e1,       = Average(e1,e2)
   let bs(e1,e2)              = Timeslet buildThresh(a,b,a_less,b_lesh(a,b,a_less,b_less)
   lossin(e1,e2)		   = Acossin(e1,e2)
   let buildCrazy3)	   = Crazy(e1, e2, e3)


   let pi = 4.0 *. atan 1.0

   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
   let rec eval x,y) = match e with
   | VarX -> x
   | VarY -> y
   | Sine(e') -> sii *. eval(e', x, y))
   | Cosine(e') -> cos(pi *. eval(e', x, y))
   | Average(e1, e2) -> ((eval(e1, x, y) +. eval(e2, x, y)) /. 2.0)
   | Times(e1, e2) ->  eval(e1, x, y) *. eval(e2, x, y)
   | Thresh(e1, e2, e3, e4) -> 
   if (eval(e1, x, y) < eval(e2, x, y)) then 
   eval(e3, x, y) 
   else 
   eval(e4, x, y)
   | Acossin(e1, e2) -> (acos(eval(e1, x, y)) *. 
   asin(eval(e2, x, y))) *. 2.0 /. (pi *. pi)
   | Crazy(e1, e2, e3) -> 
   let res1 = eval(e1, x, y) in
   let res2 = eval(e2, x, y) in
   let res3 = eval(e3, x, y) in
   if (res1 > res2) then
   (res1 +. res2 +. res3) /. 3.0
   else if (res2 > res3) then
   (res. res2 +. res3) /. 2.0
   else
   ((-1.0) *. res3)
   ;;


   let eval_fn e (x,y) = 
   letv = eval (e,y) in
   assert-1.0 <= rv && rv <= 1.0);
   rv
   ;;


   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
X*)


 ************** Functions you need to write **)

(*XXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXX
*)

let rec and, depth) = 
if deptn
let bin_rand = rand(0, 2) inrand = 0 then buildX()
else  (*XXXXXXXXXXXXXX*)
else 
  letd = r in
let first_forced = build(rand, depth - 1)n
                     match exp_rand with
                       | 0buildSine(first_forced)
                       | 1 -> buisine(first_forced)
                       | 2 -> buildAverage(first_forced, buld(rand, depth - 1))
                       | 3 -> buildTimes(first_forced, build(rand, depth- 1))
                       | 4 -> buildThresh(first_forced, bild(rand, depth - 1), 
                                          build(rand, depth - 1), build(rand, depth - 1))
                       | 5 -> buildAcossin(first_forced, build(rand, depth - 1))
                       | _ -> buildCrazy(ft_forced, first_forced, first_forced)
;;


(*XXXXXXXXXXXXX*)
lg1 () = (2, 111, 1005);;
let g2 () = (6, 1315, 666);;
let g3 ()(8, 543, 335241);;

let c1 () = (8, 1, 15;;
             let c2 () = (10, 510, 150);;
             let c3 () = (5, 3000, 39242);;

             (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

             (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
             *)

             Rand (seed1, seed2) = 
             let seed = (Array.of_list [seed1;se
                         let s = Random.State.make seed in
                           (fun (x,y) -> (x + (Rate.int s (y-x))))


                         let rec rseq g r n =
                           if n <= 0 then [1))

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*X
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let toReal (i,n) = (float_of_int i) /. (float_of_int n)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let toIntensity z = int_of_float (127.5 +. (127.5 *. z))


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*)

let rec ffor (low,high,f) = 
  iow > high then () else 
  let _ = f low in 
    ffor (low+1,h,f)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
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
                      le = toReal(iy,n) in
  (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let z =(x,y) in
  (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let iz = toIntensi(z) in
    (*XXXXXXXXXXXXXXXXXXXXXXXXXX*)
    output_can (char_of_int iz))) in 
  close_out chan;
  ignore(Sys.command ("convert "^fname^gm "^fname^".jpg"));
ignore(Sys.command ("rm "^fname^"."))

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

let doRandomGray (depth,seed1,s2) =
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let g = makeRand(seed1,seed2) in
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let e = build (g,depth)n
let _ = print_string (exprToString ein
let f = eval_fn e in
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXX*)
let name = Format.sprintf "%d_%d_%d" depth seed1 seed2 in
emirayscale ,name)

(*XXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXX

XXXXXXXXX*)


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
X*)
   let emitColor (f1,f2,f3,n,name) =Open the output file and write the header *)
   let fname  = ("art_c_"^name) in
   let cha open_out (fname^".ppm") in
   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
   let n2p1 = n*2+1 in   
   let _ = output_string n (Format.sprintf "P6 %d %d 255\n" n2p1 n2p1) in
   let _ = 
   ffor (-n, n, 
   fun ix ->
   ffo(-n, n, 
   fun iy ->
   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
   let x = toReal(ix,n)n
   let y = toReal(iy,n) in
   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
   let  = f1 (x,y) in
   let z2 = f2 ,y) in
   let z3 = f3 (x,y) in

   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
   let iz1 = toInnsity(z1) in
   leiz2 = toIntensity(z2) ilet iz3 = toIntensity(z3) in

   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
   output_char chan (char_of_int iz1);
   output_char chan (char_oft iz2);
   output_char chan (char_of_int iz3);
   )) in  
   close_out chan;
   ignore(Sys.command ("convert "^fname^".ppm  "^fname^"."));
   ignore(Sys.command ("rm "^fe^".ppm")) 

   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXX
X*)
   let doRandomColor (de,seed1,seed2) =
   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
   let g = makeRand (seed1,seed2) in
   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
   let e1 = build (g, pth) in
   let e2 = build (g, depth) in
   let e3 = bud (g, depth) in

   let _ = Format.intf "red   = %s \n" (exprToString e1)n
   let _ = Format.printf "green = %s \n" (exprString e2) in
   let _ = Fmat.printf "blue  = %s \n" (exprToString e3) in

   let f1 = eval_fn e1 in
   lef2 = evale2 in
   let f3 =3 in

   (*XXXXXX*)
   let n =e picture .sprintf "%d_%d_%d" depth seed1 seed2 i3,n,name)


   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
   (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


   (*XXXXXXXXXXXXX*)"william",[("ranjit",8moose",44)]);;    

   let _ = assoc (-1,"bob",[(am",23);("moose",44)]);;

   let123, [(123, "sad"); (321, "hahuge input *) 
let long_list = = 
if n < 100 then
  assemble (n+1) ((string_oe
                     l
in
  assemble 1 []
;;


let _ = ast)



(*XXXXXXXXXXXXX*)

let _ = rem;12;2;13;6;9];;
let _ = removeDuplic


(*XXXXXXXXX*)

let f let xx = x*x*x in (xx, xx < 100) in
  wwhile (f, 2);;


(*XXXXXXXXX*)

let g x = truncate (1e6 *. cos (1e-6 *. float x)) in fixpoint (g, 0);; 

let cotz n = match n with 1 -> 1 | _ when n mod 2 = 0 -> n/2 | _ ->n + 1;;

let _ = fixpoint (collatz, 1) ;;
let _ = fixpt (collatz, 3) ;;
let _ = fixpoint (collatz, 48) ;;
let _ =xpoint (collatz, 107) ;;
let _ = fixpoint (collatz, 9001) ;;



(*XXXXXXXXXXX*)

let sampleExpr1 = Thresh(VarXarY,VarX,(Times(Sine(VarX),Cosine(erage(VarX,VarY)))));;

let _ = exprToString sampleExpr1 



let sameExpr =
  buildCosine(buildSineuildTimes(buildCosine(buildAvage(buildCosine(
                                                          buildX()),buiTimes(buildCosine (buildCosine (buildAverage
                                                                                                         (buildTimes (bldY(),buildY()),buildCosine (buildX())))),
                                                                             buildCosine (buiTimes (buildSine (buildCosine
                                                                                                                 (buildY())),buildAverage (builine (buildX()), buildTis
                                                                                                                                                                 (buildX(),buildX()))))),buildY())))

              let sampExpr2 =
                buildThresh(buiX(),buildY(),budSine(buildX()),buildCone(buildY()))



              (*XXXXXXXXXXXXX*)

              let _ = eval (Sine(Ave(VarX,VarY)),0.5,-0.5);;
              let _ = eval (Sine(Average(VarX,VarY)),0.3,0.3);;
              let _ = eval (sampleExpr,0.5,0.2);;

              let _ = emitGrayscale (eval_fn sampleExpr, 150, "sample") ;;


              (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
              (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
              (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

              type test = unit -> string

              let key "" (*XXXXXXXX*)
              let pre30 = "130" (* ch)
                                  let print130 s = print_string (prefix130^"s)

                                  eonrroe of string

                                  exception TestException

                                  type result = Pass | Fail | ErrorCode of string

                                  let score = ref 0
                                  let max = ref 0
                                  let timeout = 300

                                  let runWTimeout (f,arg,out,time) = 
                                  try if compare (f arg) out = 0 then Pass else Fail
                                  wite -> (print130 ("Uncaught Exception: "^(Printexc.to_string e)); ErrorCode "exception") 

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
                                  match runWTimut(f,arg,out,timeout) with 
                                  Pass -> (score := !score + points; "[pass]"e^" "^outs^" ("^(string_of_int points)^")\n"

                                  (* explode : s+1)) in
                                  _exp 0

                                  let implode cs = 
                                  String.concat "" (List.ma.mem c ['(';' ';')'])) (explode s))

                                  let eq_realARNING: Your tests are not valid!!\n"

                                  let scoreMsg () = 
                                  Fsample: assoc 1"
                                  );
                                  (fun () -> mkTest 
                                  assoc
                                  (-1, "bob", "ranjit",85);("william",23);("moose",44)])
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
                                  (fun () -> doRandomGr (g3 ()))
                                  ()
                                  ()
                                  "sample: gray 3 : manual"
                                  );

                                  (fun  -> mkTest 
                                  (fun () -> doRandomColor (c1 ()))
                                  ()
                                  ()
                                  "sample: color 1 : manual"
                                  );
                                  (fun () -> mkTest 
                                  (fun () -> RandomColor (c2 ()))
                                  (()
                                  "sample: color 2 : nual"
                                  );
                                  (fun () -> mkTest 
                                  (fun () -> doRandColor (c3 ()))
                                  ()
                                  ()
                                  "sample: color 3 : ml"
                                  )] 

                                  let doTest f = 
                                  try f () with ex
                                  Format.sprintf "WARNING: INVALID TEST WS EXCEPTION!!: %s \n\n"
                                  (Printexc.to_string ex)

                                  let _ =
                                  letort = List.map doTest sampleTests                in
                                  let _      = List.iter print130 (report@([oreMsg()])) in
                                  let _      = print1 ("Compiled\                   in
                                  (!score, !max)

