type alphabet = A |E |I |O |U ;;
type a' = (alphabet array)*(int);;

let tvar = ([|A; E; O; I; A|],0);;
let tvar1 = ([||],0);;
let tvar2 = ([|E; A; U|],0);;

let lgh (l,e) = Array.length l;;

print_int (lgh tvar);;
print_newline ();;

let nonempty (l,e) = match (lgh (l,e)) with
	0 -> false
	| _ -> true;;

print_string (string_of_bool (nonempty tvar));;
print_newline ();;

print_string (string_of_bool (nonempty tvar1));;
print_newline ();;

let concat (l1,e1) (l2,e2) = ((Array.append l1  l2),0) ;;

print_int (lgh (concat tvar tvar2));;
print_newline ();;

let reverse (l,e) = ((Array.of_list (List.rev (Array.to_list l))),0);;

exception Empty ;;

let first (l,e) = 
	if ((lgh (l,e)) = 0) then raise Empty
	else Array.get l 0 ;;

let last (l,e) =
	let len = lgh (l,e) in
	if(len = 0) then raise Empty
	else Array.get l len-1 ;;

exception AtLast;;

let forward (l,e) = 
	if (e > lgh (l,e) -2) then raise AtLast
	else (l,e+1);;

exception AtFirst;;

let back (l,e) = 
	if(e < 1) then raise AtFirst
	else (l,e-1);;

exception TooShort;;
  
let moveTo n (l,e) = 
	if(n >= lgh (l,e)) then raise TooShort
	else (l,n);;

let replace (l,e) w = Array.set l e w ;;