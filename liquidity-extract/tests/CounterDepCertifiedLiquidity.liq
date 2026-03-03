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
type ('t,'e) result =
  Ok of 't
| Err of 'e

type rocq_msg = 
Rocq_Inc of int
| Rocq_Dec of int


type  rocq_SimpleCallCtx = (timestamp * (address * (tez * tez)))

type  storage = (int * key_hash)

type  rocq_Error = nat

type rocq_sumbool = 
Rocq_Left
| Rocq_Right


let rocq_my_bool_dec (b1 : bool) (b2 : bool) = (if b1 then fun x -> if x then Rocq_Left else Rocq_Right else fun x -> if x then Rocq_Right else Rocq_Left) b2

let rocq_inc_balance (st : storage) (new_balance : int) =  ((addInt st.(0) new_balance), st.(1))

let rocq_default_error  = 1p

let rocq_dec_balance (st : storage) (new_balance : int) =  ((subInt st.(0) new_balance), st.(1))

let rocq_counter (msg : rocq_msg) (st : storage) = match msg with 
Rocq_Inc (i0) -> (match rocq_my_bool_dec (leInt 0 i0) true with 
Rocq_Left  -> (Ok ( ([], (rocq_inc_balance st i0))))
 | Rocq_Right  -> (Err (rocq_default_error)))
 | Rocq_Dec (i0) -> (match rocq_my_bool_dec (leInt 0 i0) true with 
Rocq_Left  -> (Ok ( ([], (rocq_dec_balance st i0))))
 | Rocq_Right  -> (Err (rocq_default_error)))

let%init storage (setup : (int * key_hash)) = 
let inner (ctx : rocq_SimpleCallCtx) (setup : (int * key_hash)) = let ctx' = ctx in 
Ok (setup) in
let ctx = (Current.time (),
   (Current.sender (),
   (Current.amount (),
    Current.balance ()))) in
match (inner ctx setup) with Ok v -> v | Err e -> failwith e

let wrapper param (st : storage) = match rocq_counter param st with| Ok v -> v| Err e -> failwith e
let%entry main param st = wrapper param st
(*END*)
