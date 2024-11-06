module Ocfrac = struct

type t = {numerator:int;denominator:int}

let rec gcd a b =
if b=0 then a else gcd b (a mod b)

let fraction numerator denominator = 
if denominator = 0 then 
    failwith " denominator cannot be zero"
else let common = gcd numerator denominator
in 
{numerator= numerator / common ;
 denominator = denominator/common}

let simplify { numerator;denominator}=
let common = gcd numerator denominator 
in 
{numerator = numerator / common ;
 denominator= denominator/common}
end