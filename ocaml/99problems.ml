(*The functions that solve the 99 problems on 
http://ocaml.org/learn/tutorials/99problems.html *)

(********** 1 **********)
(* Write a function last : 'a list -> 'a option that returns the last element 
of a list. *)
let rec last = function
[] -> None
| [l] -> Some(l)
| _::b -> last b
;;

(********** 2 **********)
(* Find the last but one (last and penultimate) elements of a list.*)
let rec last_two = function
[] | [_] -> None
| a::[b] -> Some(a,b)
| _::(_::_ as l) -> last_two l
;;

(********** 3 **********)
(*Find the k'th element of a list. *)
let rec at idx = function
[] -> None
| hd::tl -> if idx=1 then Some(hd) else at (idx-1) tl
;;

(********** 4 **********)
(*Find the number of elements of a list. *)
let length l =
	let rec length_helper acc = function
	[] -> acc
	| _::tl -> length_helper (acc + 1) tl
 in length_helper 0 l;;

(********** 5 **********)
(*Reverse a list. *)
let rev l = 
	let rec rev_helper acc = function
	[] -> acc
	| hd::tl -> rev_helper (hd::acc) tl 
in rev_helper [] l
;;

(********** 6 **********)
(*Find out whether a list is a palindrome.*)

let is_palindrome list = 
	list = rev list

(********** 7 **********)
(*Flatten a nested list structure.*)
type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let flatten l = 
	let rec flatten_helper acc = function
	[] -> acc
	| One x :: tl -> flatten_helper (x::acc) tl 
	| Many x :: tl -> flatten_helper (flatten_helper acc x) tl
in List.rev (flatten_helper [] list)
;;  


(********** 8 **********)
(*Eliminate consecutive duplicates of list elements. *)
let rec compress = function
[] | [_] as l -> l
| a::(b::tl as l) -> if a=b then compress l else a::compress l
;; 

(********** 9 **********)
(*Pack consecutive duplicates of list elements into sublists. *)

let pack list = 
	let rec aux current listOfLists = function
	[] -> []
	| [x] -> (x::current)::listOfLists
	| a::(b::_ as tl) ->
		if a = b then aux (a::current) listOfLists tl
		else aux [] ((a::current)::listOfLists) tl
	in List.rev (aux [] [] list)

(********** 10 **********)
(*Run-length encoding of a list. *)

let encode list = 
	let rec aux rLength acc = function
	[] -> []
	|[x] -> (rLength + 1,x) :: acc 
	| a::(b::_ as t) ->
		if a = b then aux (rLength + 1) acc t
		else aux 0 ((rLength + 1 , a)::acc) t
	in List.rev( aux 0 [] list)

(********** 11 **********)
(*Modified run-length encoding. *)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;


