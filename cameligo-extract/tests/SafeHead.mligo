

let safe_head (l : nat list) : nat = 
(match l with 
[]  -> (fun (a : unit) -> (failwith 0 : nat) (* absurd case *))
 | hd :: tl -> (fun (a : unit) -> hd)) ()

let head_of_list_2 (xs : nat list) : nat = 
safe_head (0n :: (0n :: xs))

let main (st : unit * nat option) : operation list * (nat option)  = (([]: operation list), Some (head_of_list_2 ([] : nat list)))
