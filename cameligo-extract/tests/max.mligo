let lebN (a : nat) (b : nat) = a <= b

let bool_rect(type p) (f : p) (f0 : p) (b : bool) : p = 
if b then f else f0

let my_stupid_if(type a) (cond : bool) (t_branch : a) (f_branch : a) : a = 
bool_rect t_branch f_branch cond

let max_nat (n : nat) (m : nat) : nat = 
my_stupid_if (lebN n m) m n

let main (st : unit * nat option) : operation list * (nat option) = (([]: operation list), Some (max_nat 2n 3n))
