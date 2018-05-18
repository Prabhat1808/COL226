(*
	Assumptions:
	1. As stated in the problem statement, it is assumed that the input to the create function is an OCaml string
	2. If strings containing elements of 'alphabet' are to be created, then it must be done manually by the user in the source code
	3. If a new datatype 'alphabet' has to be declared then it also has to be done manually by the user in the source code

	Signatures:
	1. val lgh : 'a a' -> int = <fun>
	2. val nonempty : 'a a' -> bool = <fun>
	3. val concat : 'a a' -> 'a a' -> 'a a' = <fun>
	4. val reverse : 'a a' -> 'a a' = <fun>
	5. val first : 'a a' -> 'a = <fun>
	6. val last : 'a a' -> 'a = <fun>
	7. val create : string -> char a' = <fun>
	8. val forward : 'a a' -> 'a a' = <fun>
	9. val back : 'a a' -> 'a a' = <fun>
	10. val moveTo : int -> 'a a' -> 'a a' = <fun>
	11. val replace : 'a -> 'a a' -> 'a a' = <fun>

	Proofs:
	Assume a string to be an ordered set, with its elements indexed from 0 to l-1, l being the length of the string.
	Length of a string is the number of characters that are present in it. These characters are indexed, starting from 0 to l-1, where l is the length of the string.
	To prove that the length of two strings are equal, we provide mapping from the indices of one string to another string and prove that its one to one. 
	If it is one to one, then the length of the string is the same.
	1. lgh (concat s1 s2) = (lgh s1) + (lgh s2)
		In this case, assume a mapping(from the union of marked elements of s1 and s2 to s3) that first starts taking the indices of s1, from 0 and matches them to the the indices of the concatenated string, starting from 0.
		After s1 is exhausted, start taking indices of s2, starting from 0, and match them to the subsequent indices of the concatenated string.
		When s2 is exhausted, all the elements of the concatenated string have also been exhausted(since it just added the elements of s2 after the last element of s1).
		Therefore, the mapping we considered is one-one and therefore, the lengths are equal.
	2. lgh (reverse s) = lgh s
		The mapping to be considered in this case is,map (i)th index of s to ((lgh s)-1-i)th index of s, (i) starting from 0 to (lgh s) - 1.
		While doing this, each (i) is mapped to a unique element and all the indices of s have been mapped. 
		And while implementing this mapping, we have essentially reversed the existing string. Therefore, this mapping is one-one.
		Hence the length of the reversed string is equal to the length of the original string.
	3. lgh (replace w s) = lgh s
		The length of a string is independent of the value that is stored at a prticular index. It is only affected by the number of places occupied.
		Thus, even if we replace a character at one index by another character, that index is still occupied. Hence, the number of occupied places i still the same.
		Therefore ,the length of the two strings is still the same.

	Demo:
	At the end of this program, a commented code is written. If it is uncommented and compiled along with the source code, it checks the above three statements.
	The result comes out to be true in all the cases.
*)

type alphabet = char ;;

(*1*)
type 'a a' = {mutable s :'a array; mutable p : int};;



(*2*)
let lgh ar = Array.length ar.s;;



(*3*)
let nonempty ar = match (lgh ar) with
	0 -> false
	| _ -> true;;


(*4*)
let concat ar ar2 = {s=(Array.append ar.s ar2.s);p=0} ;;


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