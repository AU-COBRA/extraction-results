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

type msg = 
Inc of int
| Dec of int


type  simpleCallCtx = (timestamp * (address * (tez * tez)))

type  storage = (int * key_hash)

type  error = nat

let inc_balance (st : storage) (new_balance : int) =  ((addInt st.(0) new_balance), st.(1))

let default_error  = 1p

let dec_balance (st : storage) (new_balance : int) =  ((subInt st.(0) new_balance), st.(1))

let counter (msg : msg) (st : storage) = match msg with 
Inc (i0) -> (if leInt 0 i0 then Ok ( ([], (inc_balance st i0))) else Err (default_error))
 | Dec (i0) -> (if leInt 0 i0 then Ok ( ([], (dec_balance st i0))) else Err (default_error))

let%init storage (setup : (int * key_hash)) = 
let inner (ctx : simpleCallCtx) (setup : (int * key_hash)) = let ctx' = ctx in 
Ok (setup) in
let ctx = (Current.time (),
   (Current.sender (),
   (Current.amount (),
    Current.balance ()))) in
match (inner ctx setup) with Ok v -> v | Err e -> failwith e

let wrapper param (st : storage) = match counter param st with| Ok v -> v| Err e -> failwith e
let%entry main param st = wrapper param st
(*END*)
