(*The functions that solve the 99 problems on 
http://ocaml.org/learn/tutorials/99problems.html *)

(********** 1 **********)
let rec last = function
[] -> None
| [l] -> Some(l)
| _::b -> last b
;;

(********** 2 **********)
let rec last_two = function
[] | [_] -> None
| a::[b] -> Some(a,b)
| _::(_::_ as l) -> last_two l
;;

(********** 3 **********)
let rec at idx = function
[] -> None
| hd::tl -> if idx=1 then Some(hd) else at (idx-1) tl
;;

(********** 4 **********)
let length l =
	let rec length_helper acc = function
	[] -> acc
	| _::tl -> length_helper (acc + 1) tl
 in length_helper 0 l;;

(********** 5 **********)
let rev l = 
	let rec rev_helper acc = function
	[] -> acc
	| hd::tl -> rev_helper (hd::acc) tl 
in rev_helper [] l
;;

(********** 6 **********)
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

(********** 7 **********)

let rec compress = function
[] | [_] as l -> l
| a::(b::tl as l) -> if a=b then compress l else a::compress l
;; 

(********** 8 **********)
