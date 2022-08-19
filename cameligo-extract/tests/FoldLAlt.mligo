let addN (n : nat) (m : nat) = n + m

let foldLAlt(type a b)  : unit -> unit -> (a -> b -> a) -> b list -> a -> a = 
let rec foldLAlt (a : unit) (b : unit) (f : a -> b -> a) (l : b list) (a0 : a) : a = 
match l with 
[]  -> a0
 | t0 :: b0 -> (foldLAlt () () f b0 (f a0 t0))
 in (foldLAlt : unit -> unit -> (a -> b -> a) -> b list -> a -> a)

let sumAlt (xs : nat list) : nat = 
foldLAlt () () addN xs 0n

let main (st : unit * nat option) : operation list * (nat option)  = (([]: operation list), Some ( sumAlt([1n;2n;3n])))
