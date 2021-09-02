(*START*)
let[@inline] fst (p : 'a * 'b) : 'a = p.(0)
let[@inline] snd (p : 'a * 'b) : 'b = p.(1)
let[@inline] addInt (i : int) (j : int) = i + j
let[@inline] mulInt (i : int) (j : int) = i * j
let[@inline] subInt (i : int) (j : int) = i - j
let[@inline] leInt (i : int) (j : int) = i <= j
let[@inline] ltInt (i : int) (j : int) = i < j
let[@inline] eqInt (i : int) (j : int) = i = j
let[@inline] modInt(a : int)(b : int) : int = match a/b with | Some (_, r) -> int r | None -> 0
let rec powIntAcc((a,b,acc) : int*int*int) =
        if b <= 0 then acc
        else powIntAcc(a, (b-1), acc * a)
let powInt(a : int)(b : int) = powIntAcc(a,b,1)
type 'a sig_ = 'a
let exist_ a = a

type coq_msg = 
Coq_Inc of int
| Coq_Dec of int


type  coq_SimpleCallCtx = (timestamp * (address * (tez * tez)))

type  storage = int

type coq_sumbool = 
Coq_Left
| Coq_Right


let coq_bool_dec (b1 : bool) (b2 : bool) = (if b1 then fun x -> if x then Coq_Left else Coq_Right else fun x -> if x then Coq_Right else Coq_Left) b2

let coq_inc_counter (st : storage) (inc : ( (int) sig_)) = (addInt st ((fun x -> x) inc))

let coq_dec_counter (st : storage) (dec : ( (int) sig_)) = (subInt st ((fun x -> x) dec))

let coq_counter (msg : coq_msg) (st : storage) = match msg with 
Coq_Inc (i) -> (match coq_bool_dec true (ltInt 0 i) with 
Coq_Left  -> (Some ( ([], ((fun x -> x) (coq_inc_counter st i)))))
 | Coq_Right  -> None)
 | Coq_Dec (i) -> (match coq_bool_dec true (ltInt 0 i) with 
Coq_Left  -> (Some ( ([], ((fun x -> x) (coq_dec_counter st i)))))
 | Coq_Right  -> None)

let%init storage (setup : int) = 
let inner (ctx : coq_SimpleCallCtx) (setup : int) = let ctx_ = ctx in 
Some (setup) in
let ctx = (Current.time (),
   (Current.sender (),
   (Current.amount (),
    Current.balance ()))) in
match (inner ctx setup) with Some v -> v | None -> failwith ()

let wrapper param (st : storage) = match coq_counter param st with| Some v -> v| None -> failwith ()
let%entry main param st = wrapper param st
(*END*)
