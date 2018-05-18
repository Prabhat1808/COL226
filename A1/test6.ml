type alphabet = S of char ;;

(*1*)
type 'a stype = {mutable s :'a array; mutable p : int};;

let tvar = {s=[|S 'a'; S 'e';  S 'o';  S 'i'; S 'a'|];p=0};;
let tvar1 = {s=[|'a'|];p=0};;
let tvar2 = {s=[| S 'e'; S 'a';  S 'u'|];p=0};;

(*2*)
let lgh ar = Array.length ar.s;;

(*print_int (lgh tvar);;*)
(*print_newline ();;*)

(*3*)
let nonempty ar = match (lgh ar) with
	0 -> false
	| _ -> true;;
(*print_string (string_of_bool (nonempty tvar));;*)
(*print_newline ();;*)

(*print_string (string_of_bool (nonempty tvar1));;*)
(*print_newline ();;*)

(*4*)
let concat ar ar2 = {s=(Array.append ar.s ar2.s);p=0} ;;

(*print_int (lgh (concat tvar tvar2));;*)
(*print_newline ();;*)

(*5*)
let reverse ar = 
	ar.s <- (Array.of_list (List.rev (Array.to_list ar.s))); ar;;

exception Empty ;;

(*6*)
let first ar = 
	if ((lgh ar) = 0) then raise Empty
	else Array.get ar.s 0 ;;

(*7*)
let last ar =
	if((lgh ar) = 0) then raise Empty
	else Array.get ar.s ((lgh ar)-1) ;;

exception AtLast;;

(*8*)
let create str =
	let rec loop i = 
		if (i<0) then []
		else (loop (i-1)) @ [str.[i]]
		in
	{s = Array.of_list (loop ((String.length str)-1)); p =0};;		


(*9*)
let forward ar = 
	if (ar.p > (lgh ar) -2) then raise AtLast
	else ar.p <- ar.p +1 ; ar;;

exception AtFirst;;

(*10*)
let back ar = 
	if(ar.p < 1) then raise AtFirst
	else ar.p <- ar.p -1 ; ar;;

exception TooShort;;

(*11*)  
let moveTo n ar = 
	if(n >= lgh ar) then raise TooShort
	else ar.p <- n; ar;;

(*12*)
let replace w ar = Array.set ar.s ar.p w ; ar;;

(*To check the proofs given in part 4, 5 and 12, uncomment the code below
let v1 = create "Prabhat";;
let v2 = create "Kanaujia";;
lgh v1 = lgh (replace 'R' v1);;
lgh (concat v1 v2) = (lgh v1) + (lgh v2);;
lgh v1 = lgh (reverse v1);;
*)