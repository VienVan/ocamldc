(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(* Yo Akobir: cmp BRUTEFORCE is working, but optimized isn't *)
(* if you want to run sub and add, use the brute force cmp *)
open Printf

module Bigint = struct

	type sign     = Pos | Neg
	type bigint   = Bigint of sign * int list
	let  radix    = 10
	let  radixlen =  1

(* give short name to slightly longer names *)
	let car       = List.hd
	let cdr       = List.tl
	let map       = List.map
	let reverse   = List.rev
	let strcat    = String.concat
	let strlen    = String.length
	let strsub    = String.sub
	let zero      = Bigint (Pos, [])
	let one 	  = Bigint (Pos, [1])


	(* check which list of values are bigger *)
	(* optimized (O(lg(n))) *)
	(* let rec add' list1 list2 carry = match (list1, list2, carry) with *)
	let rec cmp (value1 : int list) (value2 : int list) comp = match (value1, value2, comp) with
		| value1, [], comp 			 	->  1  (* first list compared to empty list, greater*)
		| [], value2, comp 			 	-> -1  (* second list compared to empty list, less than*)
		| [], [], comp 	 			 	->
			if comp = 1 then 1
			else if comp = -1 then -1
			else 0
			(* TODO: fix this expression *)
		| car1::cdr1, car2::cdr2, comp	->
				(* (if first digit is bigger, pass on greater until the end of the list) *)
				if car1 > car2
				then cmp cdr1 cdr2 1
				else if car1 < car2 then cmp cdr1 cdr2 -1
				else cmp [] [] comp
				(* else if car1 < car2 *)
				(* then (printf "%s\n%!" "car1 < car2"; -1) *)
				(* else 0 *)

	(* brute force (O(n)) *)
	(* let rec cmp value1 value2 =
		let len1 = List.length value1 in
		let len2 = List.length value2 in
		if len1 > len2 then 1
		else if len1 < len2 then -1
		else
		(
			let val1 = reverse value1 in
			let val2 = reverse value2 in
			if (car val1) > (car val2) then 1
				else if (car val1) < (car val2) then -1
				else if (car val1 = 0) && (car val2 = 0) then 0
				else cmp (cdr val1) (cdr val2)
		) *)

	let charlist_of_string str =
		let last = strlen str - 1
		in  let rec charlist pos result =
			if pos < 0
			then result
			else charlist (pos - 1) (str.[pos] :: result)
		in  charlist last []

(* given a string, return bigint  *)
	let bigint_of_string str =
		let len = strlen str
		in  let to_intlist first =
				let substr = strsub str first (len - first) in
				let digit char = int_of_char char - int_of_char '0' in
				map digit (reverse (charlist_of_string substr))
			in  if   len = 0
				then zero
				(* code to handle negative number , no need to write *)
				else if   str.[0] = '_'
					 then Bigint (Neg, to_intlist 1)
					 else Bigint (Pos, to_intlist 0)

	let string_of_bigint (Bigint (sign, value)) =
		match value with
		| []    -> "0"
		| [-0]  -> "0"
		| value -> let reversed = reverse value
				   in  strcat "" (
					   (if sign = Pos then "" else "-") :: (map string_of_int reversed)
					   )

(* every variable under rec to tell the interpreator to treat them with recursive def *)
(* match: special syntax to compare syntc sugar for case switch / if /else *)
	let rec add' list1 list2 carry = match (list1, list2, carry) with
		| list1, [], 0       -> list1
		| [], list2, 0       -> list2
		(* without "rec" , the add' underneath would be undefined *)
		| list1, [], carry   -> add' list1 [carry] 0
		| [], list2, carry   -> add' [carry] list2 0
		| car1::cdr1, car2::cdr2, carry ->
		  let sum = car1 + car2 + carry
		  in  sum mod radix :: add' cdr1 cdr2 (sum / radix)


	let rec sub' list1 list2 carry = match (list1, list2, carry) with
		| list1, [], 0       -> list1
		| [], list2, 0       -> list2
		| list1, [], carry   -> sub' list1 [carry] 0
		| [], list2, carry   -> sub' [carry] list2 0
		| car1::cdr1, car2::cdr2, carry ->
			  let diff = car1 - car2 - carry in
			  if diff < 0 then (diff + 10) mod radix :: sub' cdr1 cdr2 1
			  else (car1 - car2 - carry) mod radix :: sub' cdr1 cdr2 0

	let add (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
		if sign1 = sign2
		then Bigint (sign1, add' value1 value2 0)
		else Bigint (sign1, sub' value1 value2 0)

	(* TODO: fix this shit  *)
	let sub (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
		(*  +a - +b, need to check which is bigger*)
		if (sign1 = Pos) && (sign2 = Pos)
		then (
			if value1 = value2 then zero
			else if (cmp value1 value2) = 1
				then (printf "%s\n%!" "value1 > value2";
					 Bigint (Pos, sub' value1 value2 0))
				else if (cmp value1 value2) = -1
					then (printf "%s\n%!" "value1 < value2";
					Bigint (Neg, sub' value2 value1 0))
					else zero
			)

			(* TODO: handle making sure big number subtract smalller,then place neg sign in front *)
			(* then Bigint (sign1, sub' value1 value2 0) *)
			(* TODO: handle - - *)
			else if (sign1 = Neg) && (sign2 = Neg)
				then Bigint (sign1, add' value1 value2 0)
			else Bigint (sign1, add' value1 value2 0)
(*
	let mul (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
		if sign1 = neg2
		then Bigint (sign1, mul' value1 value2 0)
		else zero *)

(* TODO: sub *)
(* what we're going to be doing:
	take interface function and worker function
	add: if they're the same sign, do regular add(add')
		different sign: subtract them, the smaller one from the bigger one
*)
	let sub = sub

	let mul = add

	let div = add

	let rem = add

	let pow = add

end
