type alphabet = A |E |I |O |U ;;
type a' = alphabet array;;

let tvar = [|A; E; O; I; A|];;
let tvar1 = [||];;
let tvar2 = [|E; A; U|];;

let lgh l = Array.length l;;

print_int (lgh tvar);;
print_newline ();;

let nonempty l = match (lgh l) with
	0 -> false
	| _ -> true;;

print_string (string_of_bool (nonempty tvar));;
print_newline ();;

print_string (string_of_bool (nonempty tvar1));;
print_newline ();;

let concat l1 l2 = Array.append l1  l2 ;;

print_int (lgh (concat tvar tvar2));;
print_newline ();;

let reverse l = Array.of_list (List.rev (Array.to_list l));;

exception Empty ;;

let first l = 
	if ((lgh l) = 0) then raise Empty
	else Array.get l 0 ;;

let last l =
	let len = lgh l in
	if(len = 0) then raise Empty
	else Array.get l len-1 ;;


