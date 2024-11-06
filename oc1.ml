module type XSig = sig
type t 
val add : t-> t-> t
end

module F (X:XSig) = struct
type t = X.t
let add = X.add
let rec matadd m1 m2 = match (m1 , m2) with
|[],[] -> []
|h1::t1,h2::t2 -> List.map2 add h1 h2 :: matadd t1 t2
|_ -> failwith "Matrix not of same size"
end

module XInt = struct
type t = int
let add = ( + )
end
module XFloat = struct
type t = float
let add = ( +. )
end
module XString = struct
type t = string
let add = ( ^ )
end

module AddInt = F(XInt);; (* for the sum of integer*)
module AddFloat = F(XFloat);; (*for the sum of Float*)
module AddStr = F(XString);;  (*for the sum of String*)

module Fraction = struct
(* n= numertator, d = d*)
(*This is to reduce the fraction to the smallest*)
type t = {n:int;d:int}
let rec gcd a b =
if b=0 then a else gcd b (a mod b)
let fraction n d = 
if d = 0 then 
    failwith " d cannot be zero"
else let common = gcd n d
in (n/common),(d/common)
(* int -> int -> int*int *)
end

module AddFrac = struct
type t = int * int
let sum (a,b) (c,d) = let f=a*d+b*c in let g= d*b in ((Fraction.fraction f g))
(* int*int -> int*int -> int*int this takes two tuple and returns a tuple*)
end

(*This is a comment:
after loading it in utop
for sum of matrix of int:
AddInt.matadd [[4;5];[6;7]] [[5;6];[6;7]];;

for sum of matrix of float:
AddInt.matadd [[4.;5.];[6.;7.]] [[5.;6.];[6.;7.]];;

for sum of string:
AddStr.matadd [["dsfg"];["sdf"]] [["gd"];["dfghds"]];;

all the above matrices can be of any length

For the sum of fractions:
AddFrac.sum (4,8) (5,65);;
note: this one takes two tuple with two elements as there are only two part of a fraction.
The fraction is inb form of tuple.
here (4,4) means 4/4 = 1 that would give (1,1) same as that (4,8)=4/8= 1/2 that would give (1,2)
*)