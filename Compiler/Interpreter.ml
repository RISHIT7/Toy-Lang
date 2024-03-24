type myBool = T | F 
;;

type exp = Num of int | Bl of myBool
| V of string
| Plus of exp * exp | Times of exp * exp
| And of exp * exp | Or of exp * exp | Not of exp
| Eq of exp * exp | Gt of exp * exp
;;

let rec ht e = match e with
| Bl e1 -> 0
| Num e1 -> 0
| V x -> 0
| Plus (e1, e2) -> 1 + (max (ht e1) (ht e2))
| Times (e1, e2) -> 1 + (max (ht e1) (ht e2))
| And (e1, e2)  -> 1 + (max (ht e1) (ht e2)) 
| Or (e1, e2)  -> 1 + (max (ht e1) (ht e2))  
| Not e1 -> 1 + (ht e1) 
| Eq (e1, e2)  -> 1 + (max (ht e1) (ht e2)) 
| Gt(e1, e2)  -> 1 + (max (ht e1) (ht e2)) 
;; 

let rec size e = match e with
| Bl e1 -> 1
| Num e1 -> 1
| V x -> 1
| Plus (e1, e2) -> 1 + ((size e1) + (size e2))
| Times (e1, e2) -> 1 + ((size e1) + (size e2))
| And (e1, e2)  -> 1 + ((size e1) + (size e2)) 
| Or (e1, e2)  -> 1 + ((size e1) + (size e2))  
| Not e1 -> 1 + (size e1) 
| Eq (e1, e2)  -> 1 + ((size e1) + (size e2)) 
| Gt(e1, e2)  -> 1 + ((size e1) + (size e2)) 
;; 

type values = N of int | B of bool;;

let [@warning "-8"] rec interpret exp rho = match exp with
| Num n -> N n
| Bl T -> B true
| Bl F -> B false
| V x -> rho x
| Plus (e1, e2) -> let N n1 = (interpret e1) and N n2 = (interpret e2) in N (n1 + n2)
| Times (e1, e2) -> let N n1 = (interpret e1) and N n2 = (interpret e2) in N (n1 * n2)
| And (e1, e2) -> let B b1 = (interpret e1) and B b2 = (interpret e2) in B (b1 && b2)
| Or (e1, e2) -> let B b1 = (interpret e1) and B b2 = (interpret e2) in B (b1 || b2)
| Not (e1) -> let B b1 = (interpret e1) in B (not b1)
| Eq (e1, e2) -> let N n1 = (interpret e1) and N n2 = (interpret e2) in B (n1 = n2)
| Gt (e1, e2) -> let N n1 = (interpret e1) and N n2 = (interpret e2) in B (n1 > n2)
;;
