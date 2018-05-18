type alphabet = A |E |I |O |U ;;
type a' = {s : alphabet array; mutable p : int};;

let tvar = {s=[|A; E; O; I; A|];p=0};;
let tvar1 = {s=[||];p=0};;
let tvar2 = {s=[|E; A; U|];p=0};;

let lgh ar = Array.length ar.s;;

print_int (lgh tvar);;
print_newline ();;

let nonempty ar = match (lgh ar) with
	0 -> false
	| _ -> true;;

print_string (string_of_bool (nonempty tvar));;
print_newline ();;

print_string (string_of_bool (nonempty tvar1));;
print_newline ();;

let concat ar ar2 = {s=(Array.append ar.s ar2.s);p=0} ;;

print_int (lgh (concat tvar tvar2));;
print_newline ();;

let reverse ar = (Array.of_list (List.rev (Array.to_list ar.s)));;

exception Empty ;;

let first ar = 
	if ((lgh ar) = 0) then raise Empty
	else Array.get ar.s 0 ;;

let last ar =
	if((lgh ar) = 0) then raise Empty
	else Array.get ar.s ((lgh ar)-1) ;;

exception AtLast;;

let forward ar = 
	if (ar.p > (lgh ar) -2) then raise AtLast
	else ar.p <- ar.p +1 ;;

exception AtFirst;;

let back ar = 
	if(ar.p < 1) then raise AtFirst
	else ar.p <- ar.p -1 ;;

exception TooShort;;
  
let moveTo n ar = 
	if(n >= lgh ar) then raise TooShort
	else ar.p <- n;;

let replace ar w = Array.set ar.s ar.p w ;;