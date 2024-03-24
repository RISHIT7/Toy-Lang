type myBool = T | F;;

type exp = 
  Num of int | Bl of myBool | V of string (* Values *)
| Plus of exp * exp | Times of exp * exp | Sub of exp * exp (* Arithmetics *)
| And of exp * exp | Or of exp * exp | Not of exp (* Boolean Arithmetics *)
| Eq of exp * exp | Gt of exp * exp (* Comparison Operators *)
| IfTE of exp * exp * exp | Case of exp * (exp list) (* Conditionals *)
| Pair of exp * exp | Fst of exp | Snd of exp (* Pair *)
| Lexp of defs * exp

and defs = Adef of string * exp | Pdef of defs * defs 
          | Sdef of defs * defs  | Ldef of defs * defs (* definitions and local definitions *)
;;

type values = N of int | B of bool | P of values * values;;

type opcode =   LDN of int | LDB of bool | LOOKUP of string
              | PLUS | TIMES | SUBTRACT
              | AND | OR | NOT | EQ | GT 
              | COND of opcode list * opcode list | CASE of (opcode list) list
              | PAIR | FST | SND
              | LEXP of defs | NI
;;

let myBool2bool b = match b with 
| T -> true
| F -> false
;;

exception Stuck of ((string * values) list) * values list * opcode list;; 
exception Invalid_Var of string;;
exception Invalid_Def of defs;;

let rec get_elements_until_ni lst =
   match lst with
   | [] -> []
   | NI :: _ -> []
   | head :: tail -> head :: get_elements_until_ni tail
;;

let rec compile e = match e with 
(* Values *)
| Num n  -> [LDN n] 
| Bl b -> [LDB (myBool2bool b) ]
| V x -> [LOOKUP x]
(* Arith *)
| Plus (e1, e2)  ->  (compile e1) @ (compile e2) @ [PLUS] 
| Times (e1, e2)  ->  (compile e1) @ (compile e2) @ [TIMES]  
| Sub (e1, e2)  ->  (compile e1) @ (compile e2) @ [SUBTRACT]  
(* Boolean Arith *)
| And (e1, e2)  ->  (compile e1) @ (compile e2) @ [AND] 
| Or (e1, e2)  -> (compile e1) @ (compile e2) @ [OR]  
| Not e1 -> (compile e1) @ [NOT] 
(* Comparison Ops *)
| Eq (e1, e2)  -> (compile e1) @ (compile e2) @ [EQ] 
| Gt(e1, e2)  -> (compile e1) @ (compile e2) @ [GT] 
(* Conditionals *)
| IfTE(e0, e1, e2) -> (compile e0) @ [COND(compile e1, compile e2)]
| Case(e0, le1) -> (compile e0) @ [CASE( List.map compile le1)]
(* Pairs *)
| Pair(e1, e2) -> (compile e1) @ (compile e2) @ [PAIR]
| Fst(e1) -> compile e1 @ [FST]
| Snd(e1) -> compile e1 @ [SND]
(* Let def in exp ni *)
| Lexp(d1, e1) -> [LEXP (d1)] @ (compile e1) @ [NI]
;;

let rec stkmc g s c = match s, c with 
(* Values *)
| v::_, [ ] -> v
| s, (LDN n)::c' -> stkmc g ((N n)::s) c' 
| s, (LDB b)::c' -> stkmc g ((B b)::s) c' 
| s, (LOOKUP x)::c' -> stkmc g ((List.assoc x g)::s) c'
(* Arith *)
| (N n2)::(N n1)::s', PLUS::c' -> stkmc g (N(n1+n2)::s') c' 
| (N n2)::(N n1)::s', TIMES::c' -> stkmc g (N(n1*n2)::s') c'  
| (N n2)::(N n1)::s', SUBTRACT::c' -> stkmc g (N(n1-n2)::s') c'
(* Boolean Arith *)
| (B b2)::(B b1)::s', AND::c' -> stkmc g (B(b1 && b2)::s') c' 
| (B b2)::(B b1)::s', OR::c' -> stkmc g (B(b1 || b2)::s') c' 
| (B b1)::s', NOT::c' -> stkmc g (B(not b1)::s') c' 
(* Comparison Ops *)
| (N n2)::(N n1)::s', EQ::c' -> stkmc g (B(n1 = n2)::s') c' 
| (N n2)::(N n1)::s', GT::c' -> stkmc g (B(n1 > n2)::s') c'
(* Conditionals *)
| (B true)::s', COND(c1, c2)::c' -> stkmc g s' (c1@c')
| (B false)::s', COND(c1, c2)::c' -> stkmc g s' (c2@c')
| (N n)::s', CASE(lc1)::c' -> stkmc g s' ((List.nth lc1 n)@c')
(* Pairs *)
| (a)::(b)::s', PAIR::c' -> stkmc g (P(b, a)::s') c'
| (P (a, b))::s', FST::c' -> stkmc g (a::s') c'
| (P (a, b))::s', SND::c' -> stkmc g (b::s') c'
(* Let def in exp ni *)
| s, LEXP(d1)::c' ->
   let c1, rest_c = List.partition (fun x -> x <> NI) c' in
   let a = stkmc (ret_gamma d1 g) [] (c1 @ [NI]) in 
   stkmc g (a::s) rest_c
| s, NI::c' -> stkmc g s c'
(* Error *)
| _, _ -> raise (Stuck (g, s, c)) 

and ret_gamma (definition : defs) (prev_gamma : (string*values) list) : (string*values) list = 
   let rec ret_gamma_helper definition prev_gamma = match definition with
   | Adef(a, b) -> [(a, stkmc prev_gamma [] (compile b))]
   | Pdef(d1, d2) -> (ret_gamma_helper d1 prev_gamma)@(ret_gamma_helper d2 prev_gamma)
   | Sdef (d1, d2) ->
      let val_d1 = ret_gamma_helper d1 prev_gamma in
      val_d1@(ret_gamma_helper d2 (val_d1 @ prev_gamma))
   | Ldef (d1, d2) ->
      let val_d1 = ret_gamma_helper d1 prev_gamma in
      (ret_gamma_helper d2 (val_d1 @ prev_gamma))
in (ret_gamma_helper definition prev_gamma)@prev_gamma
;;

let test1a = Plus (Times (Num 3, Num 4), Times (Num 5, Num 6));; 
let test1b = Times (Sub (Num 3, Num 4), Times (Num 3, Num 4));; 
let test2 = Or (Not (Bl T), And (Bl T, Or(Bl F, Bl T)));; 
let test3 = Gt (Times (Num 5, Num 6), (Times (Num 3, Num 4)));; 
let test4 = And (Eq(test1a, Num 42), Not test3);;
let test5a = Plus (Times (Plus (Num 1, V "x"), Num 4), Times (Num 5, Num 6));;
let test5b = Plus (Times (Plus (Num 1, V "z"), Num 4), Times (Num 5, Num 6));;

let test6 = IfTE (test3, test1a, test2);;
let test7 = IfTE (test3, test2, test1a);;
let test8 = IfTE (Not test3, test1a, test2);;
let test9 = IfTE (Not test3, test2, test1a);;

let test10 = Pair ( Pair (test1a, test1b), test1b);;
let test11 = Fst (test10);;
let test12 = Snd (test10);;

let test13a = Num 3;;
let test13b = Case (test13a, [test1a; test1b; test2; test3; test4; test5a]);;
let test13c = Case (test13a, [test1a; test1b]);;

let test14a = Adef("x", test1a);;
let test14b = Adef("x", test5a);;
let test15a = Adef("z", test1a);;
let test15b = Adef("z", test5a);;
let test16a = Pdef(test14a, test15a);; (* 42, 42 *)
let test16b = Pdef(test14b, test15b);; (* 62, 62 *)
let test17a = Sdef(test14a, test15a);; (* 42, 42 *)
let test17b = Sdef(test14b, test15b);; (* 62, 282 *)
let test18a = Ldef(test14a, test15a);; (* 42 *)
let test18b = Ldef(test14b, test15b);; (* 282 *)

let test19 = Lexp(test15a, Plus(V "z", V "x"));;

let gamma = [("x", N 2); ("y", B true); ("x", N 3)];;
let compiler = compile test19;;
let stack = stkmc gamma [] compiler;;

let gamma_test = ret_gamma test18b [("x", N 7)];;
