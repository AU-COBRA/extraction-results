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
let[@inline] andb (a : bool ) (b : bool ) = a & b
let[@inline] orb (a : bool ) (b : bool ) = a || b

type op = 
Add
| Sub
| Mult
| Lt
| Le
| Equal


type instruction = 
IPushZ of int
| IPushB of bool
| IObs of (string * int)
| IIf
| IElse
| IEndIf
| IOp of op


type value = 
BVal of bool
| ZVal of int


type  params = (( (instruction) list) * (string * int,value) map)

type  simpleCallCtx = (timestamp * (address * (tez * tez)))

type  storage = ( (value) list)

let continue_ (i : int) = eqInt i 0

let bool_to_cond (b : bool) = if b then 0 else 1

let flip (i : int) = if eqInt i 0 then 1 else if eqInt i 1 then 0 else i

let reset_decrement (i : int) = if leInt i 1 then 0 else subInt i 1

let interp  = let rec interp  (ext, insts, s, cond) = 
match insts with 
[]  -> (Some (s))
 | hd :: inst0 -> (match hd with 
IPushZ (i) -> (if continue_ cond then interp (ext, inst0, ((ZVal (i)) :: s), cond) else interp (ext, inst0, s, cond))
 | IPushB (b) -> (if continue_ cond then interp (ext, inst0, ((BVal (b)) :: s), cond) else interp (ext, inst0, s, cond))
 | IObs (p) -> (if continue_ cond then match Map.find p ext with 
Some (v) -> (interp (ext, inst0, (v :: s), cond))
 | None  -> None else interp (ext, inst0, s, cond))
 | IIf  -> (if eqInt cond 0 then match s with 
[]  -> None
 | v :: s0 -> (match v with 
BVal (b) -> (interp (ext, inst0, s0, (bool_to_cond b)))
 | ZVal (z) -> None) else interp (ext, inst0, s, (addInt 1 cond)))
 | IElse  -> (interp (ext, inst0, s, (flip cond)))
 | IEndIf  -> (interp (ext, inst0, s, (reset_decrement cond)))
 | IOp (op) -> (if continue_ cond then match op with 
Add  -> (match s with 
[]  -> None
 | v :: l -> (match v with 
BVal (b) -> None
 | ZVal (i) -> (match l with 
[]  -> None
 | v0 :: s0 -> (match v0 with 
BVal (b) -> None
 | ZVal (j) -> (interp (ext, inst0, ((ZVal ((addInt i j))) :: s0), cond))))))
 | Sub  -> (match s with 
[]  -> None
 | v :: l -> (match v with 
BVal (b) -> None
 | ZVal (i) -> (match l with 
[]  -> None
 | v0 :: s0 -> (match v0 with 
BVal (b) -> None
 | ZVal (j) -> (interp (ext, inst0, ((ZVal ((subInt i j))) :: s0), cond))))))
 | Mult  -> (match s with 
[]  -> None
 | v :: l -> (match v with 
BVal (b) -> None
 | ZVal (i) -> (match l with 
[]  -> None
 | v0 :: s0 -> (match v0 with 
BVal (b) -> None
 | ZVal (j) -> (interp (ext, inst0, ((ZVal ((mulInt i j))) :: s0), cond))))))
 | Lt  -> (match s with 
[]  -> None
 | v :: l -> (match v with 
BVal (b) -> None
 | ZVal (i) -> (match l with 
[]  -> None
 | v0 :: s0 -> (match v0 with 
BVal (b) -> None
 | ZVal (j) -> (interp (ext, inst0, ((BVal ((ltInt i j))) :: s0), cond))))))
 | Le  -> (match s with 
[]  -> None
 | v :: l -> (match v with 
BVal (b) -> None
 | ZVal (i) -> (match l with 
[]  -> None
 | v0 :: s0 -> (match v0 with 
BVal (b) -> None
 | ZVal (j) -> (interp (ext, inst0, ((BVal ((leInt i j))) :: s0), cond))))))
 | Equal  -> (match s with 
[]  -> None
 | v :: l -> (match v with 
BVal (b) -> None
 | ZVal (i) -> (match l with 
[]  -> None
 | v0 :: s0 -> (match v0 with 
BVal (b) -> None
 | ZVal (j) -> (interp (ext, inst0, ((BVal ((eqInt i j))) :: s0), cond)))))) else interp (ext, inst0, s, cond)))
 in fun ext insts s cond -> interp (ext, insts, s, cond)

let receive (p : params) (s : ( (value) list)) = let s0 = s in 
match interp p.(1) p.(0) [] 0 with 
Some (v) -> (Some ( ([], v)))
 | None  -> None

let%init storage (setup : unit) = 
let inner (ctx : simpleCallCtx) (setup : unit) = let ctx0 = ctx in 
let setup0 = setup in 
Some ([]) in
let ctx = (Current.time (),
   (Current.sender (),
   (Current.amount (),
    Current.balance ()))) in
match (inner ctx setup) with Some v -> v | None -> failwith ()

let wrapper param (st : storage) = match receive param st with| Some v -> v| None -> failwith ()
let%entry main param st = wrapper param st
(*END*)
