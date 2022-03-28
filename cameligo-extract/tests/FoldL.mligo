let addN (n : nat) (m : nat) = n + m

let foldL(type a b) (f : a -> b -> a) : b list -> a -> a = 
let rec foldL (l : b list) (a0 : a) : a = 
match l with 
[]  -> a0
 | b :: t -> (foldL t (f a0 b))
 in (foldL : b list -> a -> a)

let sum (xs : nat list) : nat = 
foldL addN xs 0n

let main (st : unit * nat option) : operation list * (nat option)  = (([]: operation list), Some ( sum([1n;2n;3n])))
