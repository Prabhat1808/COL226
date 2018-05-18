open List;;

type variable = Id of string;;

type symbol = Ele of string*int;;

type signature = symbol list;;

type term = V of variable | Node of symbol * (term list) ;;

(*checking for valid signature*)

let first_el (Ele (a,b)) = a;;
let second_el (Ele (a,b)) = b;;


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


(*substitution representation*)
let sub1 = Hashtbl.create 10;;

Hashtbl.add sub1 var1 trm1;;
Hashtbl.add sub1 var2 trm3;;
Hashtbl.add sub1 var3 trm5;;
Hashtbl.add sub1 var4 trm7;;
Hashtbl.add sub1 var5 trm9;;

(*subst : Homomorphic Extensioin*)
let rec subst s t = match t with
	V x -> if ((Hashtbl.find_all s x) = []) then
			(V x)
		else
			(Hashtbl.find s x)
	| Node (sym, tl) -> Node(sym,(map (subst s) tl));;


(*Composition : list of subs, with the first subs being executed first*)
let rec compose sublst t = match sublst with
	x :: xs -> compose sublst (subst x t)
	| [] -> t;;

let rec mgu t1 t2 = 