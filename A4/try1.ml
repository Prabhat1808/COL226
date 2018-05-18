type exp = NIL
		| C of int
		| Id of string
		| Abs of exp
		| Sum of exp * exp
		| Diff of exp * exp
		| Prod of exp * exp
		| Div of exp * exp
		| Mod of exp * exp
		| BL of bool
		| Not of exp
		| And of exp * exp
		| Or of exp * exp
		| Imply of exp * exp
		| Eq of exp * exp
		| Gt of exp * exp
		| Lt of exp * exp
		| Ge of exp * exp
		| Le of exp * exp
		| Tuple of exp list
		| Proj of int * exp
		| Lambda of string * exp
		| Call of exp * exp (* first is caller .... second is parameter*)
		| If of exp * exp * exp (*condition, case1, case2*)
		| Let of string * exp
		| LetLoc of string * exp * exp
		;;

type opcode = CONST of int
		| ID of string
		| ABS
		| SUM
		| DIFF 
		| PROD 
		| DIV
		| MOD 
		| TRUE
		| FALSE
		| NOT
		| AND 
		| OR
		| IMPLY 
		| EQ
		| GT 
		| LT 
		| GE
		| LE 
		| TUPLE of opcode list list
		| PROJ
		| CLOS of string * opcode list (*for lambda*)
		| RET
		| APP
		| COND of opcode list * opcode list
		| BIND of string
		| UNBIND of string
		;;

type answer = I of int | B of bool | L of answer list | Unit | VC of ((string, answer) Hashtbl.t) * string * opcode list;;

let tabl = Hashtbl.create 3;;
Hashtbl.add tabl "x" (I 3);;
Hashtbl.add tabl "y" (I 1);;
Hashtbl.add tabl "z" (I 5);;
Hashtbl.add tabl "w" (B true);;
Hashtbl.add tabl "v" (B false);;

let tabl_bind t var eval = Hashtbl.add t var eval ; t;;

let tabl_unbind t var = Hashtbl.remove t var; t;;


let rec compile e = match e with
		C n -> [CONST n]
		| Id s -> [ID s]
		| Abs e1 -> (compile e1) @ [ABS]
		| Sum (e1,e2) -> (compile e1) @ (compile e2) @ [SUM]
		| Diff (e1,e2) -> (compile e1) @ (compile e2) @ [DIFF]
		| Prod (e1,e2) -> (compile e1) @ (compile e2) @ [PROD] 
		| Div (e1,e2) -> (compile e1) @ (compile e2) @ [DIV]
		| Mod (e1,e2) -> (compile e1) @ (compile e2) @ [MOD]
		| BL true -> [TRUE]
		| BL false -> [FALSE]
		| Not e1 -> (compile e1) @ [NOT]
		| And (e1,e2) -> (compile e1) @ (compile e2) @ [AND]
		| Or (e1,e2) -> (compile e1) @ (compile e2) @ [OR]
		| Imply (e1,e2) -> (compile e1) @ (compile e2) @ [IMPLY]
		| Eq (e1,e2) -> (compile e1) @ (compile e2) @ [EQ]
		| Gt (e1,e2) -> (compile e1) @ (compile e2) @ [GT]
		| Lt (e1,e2) -> (compile e1) @ (compile e2) @ [LT]
		| Ge (e1,e2) -> (compile e1) @ (compile e2) @ [GE]
		| Le (e1,e2) -> (compile e1) @ (compile e2) @ [LE]
		| Tuple e1 -> [TUPLE (List.map compile e1)]
		| Proj (n, Tuple e1) -> [PROJ] @ [CONST n] @ [TUPLE (List.map compile e1)]
		| Lambda (s,e1) -> [CLOS (s, (compile e1) @ [RET])]
		| Call (e1,e2) -> (compile e1) @ (compile e2) @ [APP];
		| If (e1,e2,e3) -> (compile e1) @ [COND ((compile e2),(compile e3))]
		| LetLoc (s,e1,e2) -> (compile e1) @ [BIND s] @ (compile e2) @ [UNBIND s]
		| Let (s,e1) -> (compile e1) @ [BIND s]
	;;

exception InvalidExpr;;

(*  let rec execute (s,tabl,c) = match (s,c) with
		(s,[]) -> List.hd s
		| (s, CONST n :: c') -> execute (I n :: s , tabl,  c')
		| (s,ID s' :: c') -> execute (Hashtbl.find tabl s' :: s, tabl , c')
		| (I n1 :: s' , ABS :: c') -> execute (I (abs n1) :: s' ,tabl ,  c')
		| (I n1 :: I n2 :: s' , SUM :: c') -> execute (I (n1+n2) :: s' ,tabl , c')
		| (I n1 :: I n2 :: s' , DIFF :: c') -> execute (I (n2-n1) :: s' ,tabl , c')
		| (I n1 :: I n2 :: s' , PROD :: c') -> execute (I (n1*n2) :: s' ,tabl , c')
		| (I n1 :: I n2 :: s' , DIV :: c') -> execute (I (n2/n1) :: s' ,tabl , c')
		| (I n1 :: I n2 :: s' , MOD :: c') -> execute (I (n2 mod n1) :: s' ,tabl , c')
		| (I n1 :: I n2 :: s' , POW :: c') -> execute (I (int_of_float(float_of_int(n2) ** float_of_int(n1))) :: s' ,tabl , c')
		| (s , TRUE :: c') -> execute ( B true :: s , tabl , c')
		| (s , FALSE :: c') -> execute ( B false :: s , tabl , c')
		| (B n1 :: s , NOT :: c') -> execute (B (not n1) :: s , tabl , c')
		| (B n1 :: B n2 :: s, AND :: c') -> execute (B (n1 && n2) :: s , tabl , c')
		| (B n1 :: B n2 :: s, OR :: c') -> execute (B (n1 || n2) :: s , tabl , c')
		| (B n1 :: B n2 :: s, IMPLY :: c') -> execute (B ((not n2) || (n1 && n2)) :: s , tabl , c')
		| (I n1 :: I n2 :: s , EQ :: c') -> execute (B (n1 = n2) :: s , tabl , c')
		| (I n1 :: I n2 :: s , GT:: c') -> execute (B (n2 > n1) :: s , tabl , c')
		| (I n1 :: I n2 :: s , LT :: c') -> execute (B (n2 < n1) :: s , tabl , c')
		| (I n1 :: I n2 :: s , GE :: c') -> execute (B (n2 >= n1) :: s , tabl , c')
		| (I n1 :: I n2 :: s , LE :: c') -> execute (B (n2 <= n1) :: s , tabl , c')
		| (s , TUPLE e :: c') -> let to_tuple e1 = execute ([], tabl, e1) in
								execute (L (List.map to_tuple e) :: s , tabl, c')
		| (s , PROJ :: CONST n :: TUPLE e :: c') -> execute (execute (s,tabl,(List.nth e n)) :: s, tabl, c')
		| _ -> raise InvalidExpr;;
	;;
*)

let rec secd (stk,env,code,dump) = match (stk,code,dump) with
		(s,[],d) -> List.hd s
		| (s, CONST n :: c',d) -> secd (I n :: s , env,  c',d)
		| (s,ID s' :: c',d) -> secd (Hashtbl.find tabl s' :: s, env , c',d)
		| (I n1 :: s' , ABS :: c',d) -> secd (I (abs n1) :: s' ,env ,  c',d)
		| (I n1 :: I n2 :: s' , SUM :: c',d) -> secd (I (n1+n2) :: s' ,env , c',d)
		| (I n1 :: I n2 :: s' , DIFF :: c',d) -> secd (I (n2-n1) :: s' ,env, c',d)
		| (I n1 :: I n2 :: s' , PROD :: c',d) -> secd (I (n1*n2) :: s' ,env , c',d)
		| (I n1 :: I n2 :: s' , DIV :: c',d) -> secd (I (n2/n1) :: s' ,env , c',d)
		| (I n1 :: I n2 :: s' , MOD :: c',d) -> secd (I (n2 mod n1) :: s' ,env , c',d)
		| (s , TRUE :: c',d) -> secd ( B true :: s , env, c',d)
		| (s , FALSE :: c',d) -> secd ( B false :: s , env , c',d)
		| (B n1 :: s , NOT :: c',d) -> secd (B (not n1) :: s , env , c',d)
		| (B n1 :: B n2 :: s, AND :: c',d) -> secd (B (n1 && n2) :: s , env , c',d)
		| (B n1 :: B n2 :: s, OR :: c',d) -> secd (B (n1 || n2) :: s , env , c',d)
		| (B n1 :: B n2 :: s, IMPLY :: c',d) -> secd (B ((not n2) || (n1 && n2)) :: s , env , c',d)
		| (I n1 :: I n2 :: s , EQ :: c',d) -> secd (B (n1 = n2) :: s , env , c',d)
		| (I n1 :: I n2 :: s , GT:: c',d) -> secd (B (n2 > n1) :: s , env , c',d)
		| (I n1 :: I n2 :: s , LT :: c',d) -> secd (B (n2 < n1) :: s , env , c',d)
		| (I n1 :: I n2 :: s , GE :: c',d) -> secd (B (n2 >= n1) :: s , env , c',d)
		| (I n1 :: I n2 :: s , LE :: c',d) -> secd (B (n2 <= n1) :: s , env , c',d)
		| (s , TUPLE e :: c',d) -> let to_tuple e1 = secd ([], env, e1,d) in
								secd (L (List.map to_tuple e) :: s , env, c',d)
		| (s , PROJ :: CONST n :: TUPLE e :: c',d) -> secd (secd (s,env,(List.nth e n),d) :: s, env, c',d)

		| (s,CLOS (y,c) :: c', d) -> secd (VC (env,y,c) :: s, env, c', d)
		| (a :: VC (env1,y,c) :: s , APP :: c' , d) -> secd ([], tabl_bind env1 y a,c, (s,env,c') :: d)
		| (a :: s', RET :: c,(s,env1,c') :: d) -> secd (a :: s, env1, c', d)

		| (B true :: s, COND (c1,c2) :: c3,d) -> secd (s,env,c1 @ c3, d)
		| (B false :: s, COND (c1,c2) :: c3,d) -> secd (s,env,c2 @ c3, d)
		| (a :: s, BIND x :: c', d) -> secd (s, tabl_bind env x a, c', d)
		| (s, UNBIND x :: c', d) -> secd (s, tabl_unbind env x, c', d)
		| _ -> raise InvalidExpr;;
	;;

(********************************************************************************************************************************************)

type closure = CLOSE of exp * env and env = (string, closure) Hashtbl.t ;;

exception InvalidOperand ;;

let tabl_kr = Hashtbl.create 3;;
Hashtbl.add tabl_kr "x" (CLOSE(C 3,Hashtbl.create 1));;
Hashtbl.add tabl_kr "y" (CLOSE(C 1,Hashtbl.create 1));;
Hashtbl.add tabl_kr "z" (CLOSE(C 5,Hashtbl.create 1));;
Hashtbl.add tabl_kr "w" (CLOSE(BL true,Hashtbl.create 1));;
Hashtbl.add tabl_kr "v" (CLOSE(BL false,Hashtbl.create 1));;

let close_eval1 op in1 = match (op,in1) with
		("Abs",CLOSE (C n, envr)) -> CLOSE (C (abs n) , envr)
		| ("Not",CLOSE (BL b , envr)) -> CLOSE (BL (not b) , envr)
		| _ -> raise InvalidOperand
	;;

let rec close_eval2 op in1 in2 = match (op,in1,in2) with
		("Sum",CLOSE (C n1 , envr), CLOSE (C n2 , envr1)) -> CLOSE (C (n1 + n2) , envr)     (*NOTE : env and env1 are the same...here written different to avoid conflicts while coding*)
		| ("Diff",CLOSE (C n1 , envr), CLOSE (C n2 , envr1)) -> CLOSE (C (n1 - n2) , envr)
		| ("Prod",CLOSE (C n1 , envr), CLOSE (C n2 , envr1)) -> CLOSE (C (n1 * n2) , envr)
		| ("Div",CLOSE (C n1 , envr), CLOSE (C n2 , envr1)) -> CLOSE (C (n1 / n2) , envr)
		| ("Mod",CLOSE (C n1 , envr), CLOSE (C n2 , envr1)) -> CLOSE (C (n1 mod n2) , envr)
		| ("And", CLOSE (BL b1 , envr) , CLOSE (BL b2 , envr1)) -> CLOSE (BL (b1 && b2) , envr)
		| ("Or", CLOSE (BL b1 , envr) , CLOSE (BL b2 , envr1)) -> CLOSE (BL (b1 || b2) , envr)
		| ("Imply", CLOSE (BL b1 , envr) , CLOSE (BL b2 , envr1)) -> CLOSE (BL ((not b1) || (b1 && b2)) , envr)
		| ("Gt", CLOSE (BL b1 , envr) , CLOSE (BL b2 , envr1)) -> CLOSE (BL (b1 > b2) , envr)
		| ("Ge", CLOSE (BL b1 , envr) , CLOSE (BL b2 , envr1)) -> CLOSE (BL (b1 >= b2) , envr)
		| ("Le", CLOSE (BL b1 , envr) , CLOSE (BL b2 , envr1)) -> CLOSE (BL (b1 <= b2) , envr)
		| ("Lt", CLOSE (BL b1 , envr) , CLOSE (BL b2 , envr1)) -> CLOSE (BL (b1 < b2) , envr)
		| _ -> raise InvalidOperand
	;;

let rec krivine inp stk= match (inp,stk) with
		(CLOSE (NIL, envr) , s) -> CLOSE (NIL, envr)
		| (CLOSE (C n, envr),s) -> CLOSE (C n, envr)
		| (CLOSE (Id s1 , envr),s) -> Hashtbl.find envr s1
		| (CLOSE (Abs e , envr),s) -> close_eval1 "Abs" (krivine (CLOSE (e, envr)) s)
		| (CLOSE (Sum (e1,e2) , envr),s) -> close_eval2 "Sum" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		| (CLOSE (Diff (e1,e2) , envr),s) -> close_eval2 "Diff" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		| (CLOSE (Prod (e1,e2) , envr),s) -> close_eval2 "Prod" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		| (CLOSE (Div (e1,e2) , envr),s) -> close_eval2 "Div" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		| (CLOSE (Mod (e1,e2) , envr),s) -> close_eval2 "Mod" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		| (CLOSE (BL b , envr),s) -> CLOSE (BL b , envr)
		| (CLOSE (Not e, envr),s) -> close_eval1 "Not" (krivine (CLOSE (e, envr))  s)
		| (CLOSE (And (e1,e2) , envr),s) -> close_eval2 "And" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr)) s)
		| (CLOSE (Or (e1,e2) , envr),s) -> close_eval2 "Or" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		| (CLOSE (Imply (e1,e2) , envr),s) -> close_eval2 "Imply" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		| (CLOSE (Gt (e1,e2) , envr),s) -> close_eval2 "Gt" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		| (CLOSE (Ge (e1,e2) , envr),s) -> close_eval2 "Ge" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		| (CLOSE (Le (e1,e2) , envr),s) -> close_eval2 "Le" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		| (CLOSE (Lt (e1,e2) , envr),s) -> close_eval2 "Lt" (krivine (CLOSE (e1, envr))  s) (krivine (CLOSE (e2, envr))  s)
		(*look into tuple and projection later on*)
		| (CLOSE (Lambda (s',e) , envr), CLOSE (e', envr1) :: s) -> krivine (CLOSE (e , (tabl_bind envr s' (CLOSE (e', envr1))) ) ) s
		| (CLOSE (Call (e1,e2) , envr) , s) -> krivine (CLOSE (e1 , envr)) ( (CLOSE (e2, envr)) :: s )
		| (CLOSE (If (e1,e2,e3) , envr) , s) -> (match (krivine (CLOSE (e1, envr)) s) with 
													CLOSE (BL true, envrn) -> krivine (CLOSE (e2, envr)) s
													| CLOSE (BL false, envrn) -> krivine (CLOSE (e3, envr)) s )
		| (CLOSE (LetLoc (s',e1,e2) , envr) ,s) -> krivine (CLOSE (e2, tabl_bind envr s' (CLOSE (e1,envr)) )) s
		| (CLOSE (Let (s',e1) , envr) ,s) -> CLOSE (NIL, tabl_bind envr s' (CLOSE (e1,envr))  )
		| _ -> raise InvalidOperand
	;;



(*

let a = Sum(C (1),C (2));;
let b = Prod(C (6),C (6));;
let c = Div(b,a);;
let d = Id ("x");;
let e = Id ("z");;
let f = Mod( (Sum(Abs(Diff (e,C 6)),C 2)) , C 5);;

let b1 = BL true;;
let b2 = BL false;;
let b3 = And ((Not b1) , (Not b2));;
let b4 = Or (b1, b3);;
let b5 = Imply (b4,b2);;
let b6 = Imply (b3,b4);;
let b7 = Tuple [b1;b2;b3;b4;b5;b6];;
let b8 = Proj (5, b7);;
let b9 = And (b8,b2);;


secd ([],tabl,compile a,[]);;
krivine (CLOSE (a,tabl_kr)) [];;

secd ([],tabl,compile b,[]);;
krivine (CLOSE (b,tabl_kr)) [];;

secd ([],tabl,compile c,[]);;
krivine (CLOSE (c,tabl_kr)) [];;

secd ([],tabl,compile d,[]);;
krivine (CLOSE (d,tabl_kr)) [];;

secd ([],tabl,compile e,[]);;
krivine (CLOSE (e,tabl_kr)) [];;

secd ([],tabl,compile f,[]);;
krivine (CLOSE (f,tabl_kr)) [];;

secd ([],tabl,compile b5,[]);;
krivine (CLOSE (b5,tabl_kr)) [];;

secd ([],tabl,compile b6,[]);;
krivine (CLOSE (b6,tabl_kr)) [];;

secd ([],tabl,compile b7,[]);;
krivine (CLOSE (b7,tabl_kr)) [];;

secd ([],tabl,compile b8,[]);;
krivine (CLOSE (b8,tabl_kr)) [];;

secd ([],tabl,compile b9,[]);;
krivine (CLOSE (b9,tabl_kr)) [];;



let g = Abs(C (-1));;
let h = Proj(1, Tuple([Id ("x");C (121);Id ("x")]));;

let i = Diff (Proj(1, Tuple[C (2);C (5);C (8)]),Id ("x"));;
let j = Mod(Proj(2, Tuple[C (2);C (5);C (8)]),C (2));;


*)