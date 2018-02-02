(* $Id: maindc.ml,v 1.5 2017-04-07 13:24:41-07 - - $ *)

include Scanner
include Bigint

(* open allows us to use these that are unqualified  *)
(* that means i can use things from this module without qualifying these  *)
(* similar to using namespace std *)
open Bigint
open Printf
open Scanner

(* Stack is a module that lives in the standard ocaml library *)
(* simiar to Stack::t<Bigint::bigint> *)
type stack_t = Bigint.bigint Stack.t (* type stack_t = alias *)
let push = Stack.push
let pop = Stack.pop

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

(* if number has more than 69 characters, append new line character after 74th *)
let rec print_number number =
	let numstr = string_of_bigint number
	in let len = strlen numstr in
	if len >= 70
		then	let substr1 = String.sub numstr 0 69 in
				let substr2 = String.sub numstr 69 (len - 69) in
				printf "%s\\\n%!" substr1 ;
				print_number (bigint_of_string substr2)
	else printf "%s\n%!" (string_of_bigint number)

let print_stackempty () = printf "stack empty\n%!"

let executereg (thestack: stack_t) (oper: char) (reg: int) =
	try match oper with
		| 'l' -> printf "operator l reg 0%o is unimplemented\n%!" reg
		| 's' -> printf "operator s reg 0%o is unimplemented\n%!" reg
		| _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
	with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
	try let right = pop thestack
		in  try let left = pop thestack
				in  push (oper left right) thestack
			with Stack.Empty -> (print_stackempty ();
								 push right thestack)
	with Stack.Empty -> print_stackempty ()

(* give it operator and the stack  *)

let execute (thestack: stack_t) (oper: char) =
	try match oper with
		| '+'  -> executebinop thestack Bigint.add
		| '-'  -> executebinop thestack Bigint.sub
		| '*'  -> executebinop thestack Bigint.mul
		| '/'  -> executebinop thestack Bigint.div
		| '%'  -> executebinop thestack Bigint.rem
		| '^'  -> executebinop thestack Bigint.pow
		| 'c'  -> Stack.clear thestack
		| 'd'  -> push (Stack.top thestack) thestack
		| 'f'  -> Stack.iter print_number thestack
		| 'l'  -> failwith "operator l scanned with no register"
		| 'p'  -> print_number (Stack.top thestack)
		| 'q'  -> raise End_of_file
		| 's'  -> failwith "operator s scanned with no register"
		| '\n' -> ()
		| ' '  -> ()
		| _    -> printf "0%o is unimplemented\n%!" (ord oper)
	with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
	let scanbuf = Lexing.from_channel inputchannel in
	let rec toploop () =
		try  let nexttoken = Scanner.scanner scanbuf
			 in  (match nexttoken with
				 | Number number       -> push number thestack
				 | Regoper (oper, reg) -> executereg thestack oper reg
				 | Operator oper       -> execute thestack oper
				 );
			 toploop ()
		with End_of_file -> printf "End_of_file\n%!";
	in  toploop ()

let readfiles () =
	let thestack : bigint Stack.t = Stack.create ()
	in  ((if Array.length Sys.argv > 1
		 then try  let thefile = open_in Sys.argv.(1)
				   in  toploop thestack thefile
			  with Sys_error message -> (
				   printf "%s: %s\n%!" Sys.argv.(0) message;
				   exit 1));
		toploop thestack stdin)

let interact () =
	let thestack : bigint Stack.t = Stack.create ()
	in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()
