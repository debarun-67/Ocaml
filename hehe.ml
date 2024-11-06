module type Sig = sig
type t 
val add : t-> t-> t
end
module F (X:Sig) = struct
type t = X.t
let add = X.add
let rec matadd m1 m2 = match (m1 , m2) with
|[],[] -> []
|h1::t1,h2::t2 -> List.map2 add h1 h2 :: matadd t1 t2
|_ -> failwith "Matrix not of same size"
end
module Int = struct
type t = int
let add = ( + )
end
module Float = struct
type t = float
let add = ( +. )
end
module String = struct
type t = string
let add = ( ^ )
end
module Fraction = struct
type fraction = {numerator:int;denominator:int}
let rec gcd a b =
if b=0 then a else gcd b (a mod b)
let return e f g= e/g, f/g
let frac numerator denominator = 
if denominator = 0 then 
    failwith " denominator cannot be zero"
else let common = gcd numerator denominator
in return numerator denominator common
end
module AddInt = F(Int);;
module AddFloat = F(Float);;
module AddString = F(String);;
module AddFrac = struct
let sum (a,b) (c,d) = Fraction.frac (b*c+a*d) (d*b)
end