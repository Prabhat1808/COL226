open List;;

type variable = Id of string;;

type symbol = Ele of string*int;;

type signature = symbol list;;

type term = V of variable | Node of symbol * (term list) ;;

(*checking for valid signature*)

let first_el (Ele (a,b)) = a;;  (*to get the name of the symbol*)
let second_el (Ele (a,b)) = b;; (*to get the arity of a symbol*)


(*test cases to check validity*)
let lst3 = [Ele ("plus",4);Ele ("minus",2);Ele ("mul",2);Ele ("mod",1);Ele ("const",0)];;
let lst4 = [Ele ("plus",4);Ele ("minus",2);Ele ("mul",2);Ele ("mod",1);Ele ("const",0);Ele ("minus",3)];;
let lst5 = [Ele ("plus",4);Ele ("minus",2);Ele ("mul",2);Ele ("minus",-1);Ele ("mod",1);Ele ("const",0)];;
let lst6 = [Ele ("plus",4);Ele ("mul",2);Ele ("minus",-1);Ele ("mod",1);Ele ("const",0)];;

let rec no_dupl l = match l with
	[] -> true
	| (h :: t) ->
		let x = (filter (fun x -> x = h) t) in
			if (x == []) then
				no_dupl t
			else
				false;;

let rec no_neg l = match l with
	[] -> true
	| (h :: t) -> if (h >= 0) then
					no_neg t
				  else
				  	false;;

let check_sig l = if((no_dupl (map first_el l)) && (no_neg (map second_el l))) then
					true
				  else
				  	false;;

(*checked for valid signature*)

(*Examples to check check_sig

check_sig lst3;;
check_sig lst4;;
check_sig lst5;;
check_sig lst6;;

*)


(*checking for a well-fomed pre-term*)

let rec get_arity signat s = 
	let t = (filter (fun x -> (first_el x) = s) signat) in
		if (t = []) then -1
	else (second_el (hd t));;


let rec wfterm signat t = match t with
	V x -> true
	| Node (s,l) -> if ((get_arity signat (first_el s)) = (length l)) then
						(for_all (wfterm signat) l)
					else
						false;;

(*examples*)
let sym1 = Ele ("plus",4);;
let sym2 = Ele ("minus",2);;
let sym3 = Ele ("mul",2);;
let sym4 = Ele ("mod",1);;
let sym5 = Ele ("const",0);;

let var1 = Id "1";;
let var2 = Id "2";;
let var3 = Id "3";;
let var4 = Id "4";;
let var5 = Id "5";;

let trm1 = V var1;;
let trm2 = V var2;;
let trm3 = V var3;;
let trm4 = V var4;;
let trm5 = V var5;;

let trm6 = Node (sym1,[trm1;trm2;trm3;trm4]);;
let trm7 = Node (sym2,[trm6;trm5]);;
let trm8 = Node (sym2,[trm6;trm5;trm7]);;
let trm9 = Node (sym3,[trm7;trm6]);;
let trm10 = Node (sym4,[trm9]);;
let trm11 = Node (sym5,[]);;

let trm12 = Node (sym1 , [trm6;trm7;trm9;trm10]);;

let trmx = Node(sym2,[trm5;trm6]);;

(* Examples to check wfterm

wfterm lst3 trm5;;
wfterm lst3 trm6;;
wfterm lst3 trm7;;
wfterm lst3 trm8;;
wfterm lst3 trm12;;

*) 

(*checked for well formed term*)

(*calc height*)
let rec ht t = match t with
	V x -> 0
	| Node (s, []) -> 0
	| Node (s,l) -> 1 + hd (rev (sort compare (map (ht) l)));;

(*calc size*)
let rec size t = match t with
	V x -> 1
	| Node (s,[]) -> 1
	| Node (s,l) -> 1 + (fold_left (fun a b -> a + b) 0 (map (size) l));;

let dupl l =
  let rec rev a l =
    match l with
      | [] -> a
      | hd::tl -> rev (hd::a) (filter (fun x -> x  != hd) tl) in
  rev [] l;;

(*calculate vars*)
let rec vars t = match t with
	V x -> [x]
	| Node (s,l) -> dupl ([] @ (flatten ((map (vars) l))));;


(*To check ht, size and vars

ht trm5;;
size trm5;;
vars trm5;;

ht trm7;;
size trm7;;
vars trm7;;

ht trm11;;
size trm11;;
vars trm11;;

ht trm12;;
size trm12;;
vars trm12;;

*)

(*substitution representation*)
let sub1 = Hashtbl.create 3;;

Hashtbl.add sub1 var1 trm1;;
Hashtbl.add sub1 var3 trm5;;
Hashtbl.add sub1 var5 trm9;;

let sub2 = Hashtbl.create 2;;

Hashtbl.add sub2 var2 trm3;;
Hashtbl.add sub2 var4 trm6;;


(*Function to display the contents of the HashTable*)
let rec disp h = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [];;


(*Display the key-vallue pair of HashTable

disp sub1;;
disp sub2;;

*)

(*subst : Homomorphic Extension*)
let rec subst s t = match t with
	V x -> if ((Hashtbl.find_all s x) = []) then
			(V x)
		else
			(Hashtbl.find s x)
	| Node (sym, tl) -> Node(sym,(map (subst s) tl));;

(*To check the substitution function

subst sub1 trm7;;
subst sub2 trm7;;

*)


(*A function that returns 'unit'. Was required in the MGU function*)
let nul a = ();;


(*Composition :
Two sets of functions.
The set of two upper functions returns the composition of two hashtables without modifying the original.
The second set of functions stores the composition of two hashtables in the first hashtable and returns it.
The second one was required in the MGU as it needed to record the cumulative compositions.
*)
let rec composec sub sub2 = 
	let sub1 = (Hashtbl.copy sub) in
	Hashtbl.iter (fun key value -> (Hashtbl.replace sub1 key (subst sub2 value))) sub1; sub1;;

let rec compose sub sub2 = 
	let sub1 = (composec sub sub2) in
	Hashtbl.iter (fun key value -> if((Hashtbl.find_all sub1 key) = []) then (Hashtbl.add sub1 key value) ) sub2; sub1;;

let rec composec_in_situ sub1 sub2 = 
	Hashtbl.iter (fun key value -> (Hashtbl.replace sub1 key (subst sub2 value))) sub1; sub1;;

let rec compose_in_situ sub sub2 = 
	let sub1 = (composec_in_situ sub sub2) in
	Hashtbl.iter (fun key value -> if((Hashtbl.find_all sub1 key) = []) then (Hashtbl.add sub1 key value) ) sub2; sub1;;

(*check composition

disp (compose sub1 sub2);;

*)


(*MGU*)
exception NOT_UNIFIABLE;;

let rec mgu t1 t2 = 
	let mgu_sub = (Hashtbl.create 3) in
	match (t1,t2) with
	(V x,V y) -> (if (not (x = y)) then ((Hashtbl.add mgu_sub x (V y)))); mgu_sub
	| (V x, Node (sym, tl)) -> (if((second_el sym) = 0)
								then ((Hashtbl.add mgu_sub x (Node (sym,tl))))
							else
								(if (mem x (vars (Node(sym,tl)))) then raise NOT_UNIFIABLE
										else (((Hashtbl.add mgu_sub x (Node (sym,tl))))
								))); mgu_sub
	| (Node (sym,tl) , V x) -> mgu (V x) (Node (sym,tl))
	| (Node (sym1,tl1), Node(sym2, tl2)) -> (
											if(not (sym1 = sym2)) then
												raise NOT_UNIFIABLE
											else
												begin
													if( not ((second_el sym1) = 0)) then
														begin
															let rec unify sign l1 l2 = match (l1,l2) with
															| (x::xs , y::ys) -> unify (compose_in_situ sign (mgu x y)) (map (subst (mgu x y)) xs) (map (subst (mgu x y)) ys)
															| ([],[]) -> compose_in_situ (Hashtbl.create 1) sign
															in

														nul (unify mgu_sub tl1 tl2)
														end
												end); mgu_sub;;


(*Examples for MGU

disp (mgu trm5 trm6);;
disp (mgu trm4 trm5);;
mgu trm1 trm6;;
disp (mgu trm6 trm5);;
mgu trm6 trm7;;
disp (mgu trm7 trmx);;

*)