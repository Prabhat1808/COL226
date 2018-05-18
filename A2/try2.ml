type exp = C of int
		| Id of string
		| Abs of exp
		| Sum of exp * exp
		| Diff of exp * exp
		| Prod of exp * exp
		| Div of exp * exp
		| Mod of exp * exp
		| Pow of exp * exp
		| T
		| F
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
		;;


type answer = I of int | B of bool | L of answer list;;

let to_int t = match t with
	I n -> n;;

let to_bool t = match t with
	B b -> b;;

let rho t = match t with
	_ -> I 2;;

let rec eval rho e = match e with
		C n -> I n
		| Id s -> (rho s)
		| Abs e1 -> I(abs (to_int (eval rho e1)))
		| Sum (e1,e2) -> I ((to_int (eval rho e1)) + (to_int (eval rho e2)))
		| Diff (e1,e2) -> I ((to_int (eval rho e1)) - (to_int (eval rho e2)))
		| Prod(e1,e2) -> I ((to_int (eval rho e1)) * (to_int (eval rho e2)))
		| Div (e1,e2) -> I ((to_int (eval rho e1)) / (to_int (eval rho e2)))
		| Mod (e1,e2) -> I ((to_int (eval rho e1)) mod (to_int (eval rho e2)))
		| Pow (e1,e2) -> I ((int_of_float(float_of_int(to_int (eval rho e1)) ** float_of_int(to_int (eval rho e2)))))
		| T -> B true
		| F ->  B false
		| Not exp -> B (not (to_bool (eval rho exp)))
		| And (e1,e2) -> B ((to_bool (eval rho e1)) && (to_bool (eval rho e2)))
		| Or (e1,e2) -> B ((to_bool (eval rho e1)) || (to_bool (eval rho  e2)))
		| Imply (e1,e2) -> B ( (not (to_bool (eval rho e1)))  || ((to_bool (eval rho e1)) && (to_bool (eval rho  e2)))  )
		| Eq (e1,e2) -> B ((to_int (eval rho e1)) = (to_int (eval rho e2)))
		| Gt(e1,e2) -> B ((to_int (eval rho e1)) > (to_int (eval rho e2)))
		| Lt (e1,e2) -> B ((to_int (eval rho e1)) < (to_int (eval rho e2)))
		| Ge (e1,e2) -> B ((to_int (eval rho e1)) >= (to_int (eval rho e2)))
		| Le (e1,e2) -> B ((to_int (eval rho e1)) <= (to_int (eval rho e2)))
		| Tuple e1 -> L (List.map (eval rho) e1) 
		| Proj (n,Tuple e1) -> eval rho (List.nth e1 n)
		;;

let v1 = C 3;;
let v2 = C 5;;
let v3 = Abs (Diff (v1,v2));;
let v4 = Sum (v1,v2);;
let v4 = Prod (v3, v4);;
let v5 = Id "x";;
let v6 = Sum (v5,v4);;
let v7 = Tuple [v1;v2;v3;v4;v5;v6];;
let v8 = Proj (4,v7);;
let v9 = Sum (v4,v8);;


type opcode = CONST of int
		| ID of string
		| ABS
		| SUM
		| DIFF 
		| PROD 
		| DIV
		| MOD 
		| POW 
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
		;;

let rec compile e = match e with
		C n -> [CONST n]
		| Id s -> [ID s]
		| Abs e1 -> (compile e1) @ [ABS]
		| Sum (e1,e2) -> (compile e1) @ (compile e2) @ [SUM]
		| Diff (e1,e2) -> (compile e1) @ (compile e2) @ [DIFF]
		| Prod (e1,e2) -> (compile e1) @ (compile e2) @ [PROD] 
		| Div (e1,e2) -> (compile e1) @ (compile e2) @ [DIV]
		| Mod (e1,e2) -> (compile e1) @ (compile e2) @ [MOD]
		| Pow (e1,e2) -> (compile e1) @ (compile e2) @ [POW]
		| T -> [TRUE]
		| F -> [FALSE]
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
	;;


let rec execute (s,rho,c) = match (s,c) with
		(s,[]) -> List.hd s
		| (s, CONST n :: c') -> execute (I n :: s , rho,  c')
		| (s,ID s' :: c') -> execute (rho s' :: s, rho , c')
		| (I n1 :: s' , ABS :: c') -> execute (I (abs n1) :: s ,rho ,  c')
		| (I n1 :: I n2 :: s' , SUM :: c') -> execute (I (n1+n2) :: s' ,rho , c')
		| (I n1 :: I n2 :: s' , DIFF :: c') -> execute (I (n2-n1) :: s' ,rho , c')
		| (I n1 :: I n2 :: s' , PROD :: c') -> execute (I (n1*n2) :: s' ,rho , c')
		| (I n1 :: I n2 :: s' , DIV :: c') -> execute (I (n2/n1) :: s' ,rho , c')
		| (I n1 :: I n2 :: s' , MOD :: c') -> execute (I (n2 mod n1) :: s' ,rho , c')
		| (I n1 :: I n2 :: s' , POW :: c') -> execute (I (int_of_float(float_of_int(n2) ** float_of_int(n1))) :: s' ,rho , c')
		| (s , TRUE :: c') -> execute ( B true :: s , rho , c')
		| (s , FALSE :: c') -> execute ( B false :: s , rho , c')
		| (B n1 :: s , NOT :: c') -> execute (B (not n1) :: s , rho , c')
		| (B n1 :: B n2 :: s, AND :: c') -> execute (B (n1 && n2) :: s , rho , c')
		| (B n1 :: B n2 :: s, OR :: c') -> execute (B (n1 || n2) :: s , rho , c')
		| (B n1 :: B n2 :: s, IMPLY :: c') -> execute (B ((not n2) || (n1 && n2)) :: s , rho , c')
		| (I n1 :: I n2 :: s , EQ :: c') -> execute (B (n1 = n2) :: s , rho , c')
		| (I n1 :: I n2 :: s , GT:: c') -> execute (B (n2 > n1) :: s , rho , c')
		| (I n1 :: I n2 :: s , LT :: c') -> execute (B (n2 < n1) :: s , rho , c')
		| (I n1 :: I n2 :: s , GE :: c') -> execute (B (n2 >= n1) :: s , rho , c')
		| (I n1 :: I n2 :: s , LE :: c') -> execute (B (n2 < n1) :: s , rho , c')
		| (s , TUPLE e :: c') -> let to_tuple e1 = execute ([], rho, e1) in
								execute (L (List.map to_tuple e) :: s , rho, c')
		| (s , PROJ :: CONST n :: TUPLE e :: c') -> execute (execute (s,rho,(List.nth e n)) :: s, rho, c')
	;;