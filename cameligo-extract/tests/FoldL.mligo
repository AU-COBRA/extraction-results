let addN (n : nat) (m : nat) = n + m

let foldL(type a b) (f : a -> b -> a) : b list -> a -> a = 
let rec foldL (l : b list) (a0 : a) : a = 
match l with 
[]  -> a0
 | t0 :: b0 -> (foldL b0 (f a0 t0))
 in (foldL : b list -> a -> a)

let sum (xs : nat list) : nat = 
foldL addN xs 0n

let main (st : unit * nat option) : operation list * (nat option) = (([]: operation list), Some ( sum([1n;2n;3n])))
