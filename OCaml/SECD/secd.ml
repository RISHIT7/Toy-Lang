type myBool = T | F;;

type exp = 
            Num of int | Bl of myBool | V of string (* Values *)
          | Plus of exp * exp | Times of exp * exp | Sub of exp * exp (* Arithmetics *)
          | And of exp * exp | Or of exp * exp | Not of exp (* Boolean Arithmetics *)
          | Eq of exp * exp | Gt of exp * exp (* Comparison Operators *)
          | IfTE of exp * exp * exp | Case of exp * (exp list) (* Conditionals *)
          | Pair of exp * exp | Fst of exp | Snd of exp (* Pair *)
          | Lexp of defs * exp (* LEt Expressions *)
          | Abs of string * exp | App of exp * exp (* Abstractions *)
;;

type opcode =   
            LDN of int | LDB of bool | LOOKUP of string
          | PLUS | TIMES | SUBTRACT
          | AND | OR | NOT | EQ | GT 
          | COND of opcode list * opcode list | CASE of (opcode list) list
          | PAIR | FST | SND
          | LEXP of defs | NI
          | APP | MKCLOS of string * opcode list | RET
;;

let myBool2bool b = match b with 
| T -> true
| F -> false
;;

type codes = opcode list;;
type environment =  (string * answer) list
and answer = 
  | N of int
  | B of bool
  | P of answer * answer
  | VCLs of environment * string * codes
;;
type stack = answer list;;
type dump = (stack * environment * codes) list;;

exception Stuck of stack*environment*codes*dump;; 
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
(* Abstractions *)
  | Abs(x, e1) -> [MKCLOS(x, (compile e1) @ [RET])]
  | App(e1, e2) -> (compile e1) @ (compile e2) @ [APP]
;;

let rec stkmc s e c d = match (s, e, c, d) with
(* Values *)
  | (x::_, _, [], _) -> x
  | (_, _, LDN(n)::c', _) -> stkmc (N n::s) e c' d
  | (_, _, LDB(b)::c', _) -> stkmc (B b::s) e c' d
  | (s, _, LOOKUP(x)::c', _) -> stkmc ((List.assoc x e)::s) e c' d
(* Arith *)
  | ((N n2)::(N n1)::s', _, PLUS::c', _) -> stkmc (N(n1+n2)::s') e c' d 
  | ((N n2)::(N n1)::s', _, TIMES::c', _) -> stkmc (N(n1*n2)::s') e c' d  
  | ((N n2)::(N n1)::s', _, SUBTRACT::c', _) -> stkmc (N(n1-n2)::s') e c' d
(* Boolean Arith *)
  | ((B b2)::(B b1)::s',_ ,AND::c', _) -> stkmc (B(b1 && b2)::s') e c' d
  | ((B b2)::(B b1)::s',_ ,OR::c', _) -> stkmc (B(b1 || b2)::s') e c' d
  | ((B b1)::s',_ ,NOT::c', _) -> stkmc (B(not b1)::s') e c' d
(* Comparison Ops *)
  | ((N n2)::(N n1)::s',_ ,EQ::c', _) -> stkmc (B(n1 == n2)::s') e c' d
  | ((N n2)::(N n1)::s',_ ,GT::c', _) -> stkmc (B(n1 > n2)::s') e c' d
(* Conditionals *)
  | ((B true)::s',_ ,COND(c1, c2)::c', _) -> stkmc s' e (c1@c') d
  | ((B false)::s',_ ,COND(c1, c2)::c', _)-> stkmc s' e (c2@c') d
  | ((N n)::s',_ ,CASE(lc1)::c', _) -> stkmc s' e ((List.nth lc1 n)@c') d
(* Pairs *)
  | ((a)::(b)::s',_ ,PAIR::c', _) -> stkmc (P(b, a)::s') e c' d
  | ((P (a, b))::s',_ ,FST::c', _) -> stkmc (a::s') e c' d
  | ((P (a, b))::s',_ ,SND::c', _) -> stkmc (b::s') e c' d
(* Abstractions *)
  | (_, _, MKCLOS(x, c1)::c2, _) -> stkmc (VCLs(e, x, c1)::s) e c2 d
  | (x::VCLs(e1,x1, c1)::s', _, APP::c', _) -> stkmc [] ((x1, x)::e1) c1 ((s', e, c')::d)
  | (x::s', _, RET::c', (s1, e1, c1)::d') -> stkmc (x::s1) e1 c1 d'
(* Error *)
  | (_, _, _, _) -> raise (Stuck (s, e, c, d))
;;

let run p env = stkmc [] env (compile p) [];;

let cur_env = [("x", N 3); ("y", N 5); ("z", B true)];;
let gamma = [("x", N 2); ("y", B true); ("x", N 3)];;

let p1 = Abs("x", V "x");;
let p2 = Abs("x", Num 4);;
let p3 = App(p1, Num 3);;

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

let p4 = Abs("x", test5a);;
let p5 = App(p4, Num 7);;

(*
run p1 cur_env;; (* \x.x *)
run p2 cur_env;; (* \x.4 *)
run p3 cur_env;; (* (\x.x).3 -> 3 *)
run test1a gamma;; (* 42 *)
run test1b gamma;; (* -12 *)
run test2 gamma;; (* true *)
run test3 gamma;; (* true *)
run test4 gamma;; (* false *)
run test5a gamma;; (* 42 *)
run test5b gamma;; (* Not_found *) 
run test6 gamma;; (* 42 *)
run test7 gamma;; (* true *)
run test8 gamma;; (* true *)
run test9 gamma;; (* 42 *)
run test10 gamma;; (* ((42, -12), -12) *)
run test11 gamma;; (* (42, -12) *)
run test12 gamma;; (* -12 *)
run test13a gamma;; (* 3 *)
run test13b gamma;; (* true *)
run test13c gamma;; (* Failure nth *)
run p4 cur_env;;
run p5 cur_env;;
*)