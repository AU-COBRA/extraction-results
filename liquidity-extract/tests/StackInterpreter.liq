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
type ('t,'e) result =
  Ok of 't
| Err of 'e

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

type  error = nat

let continue_ (i : int) = eqInt i 0

let default_error  = 1p

let bool_to_cond (b : bool) = if b then 0 else 1

let flip (i : int) = if eqInt i 0 then 1 else if eqInt i 1 then 0 else i

let reset_decrement (i : int) = if leInt i 1 then 0 else subInt i 1

let interp  = let rec interp  (ext, insts, s, cond) = 
match insts with 
[]  -> (Ok (s))
 | inst00 :: hd0 -> (match inst00 with 
IPushZ (i0) -> (if continue_ cond then interp (ext, hd0, ((ZVal (i0)) :: s), cond) else interp (ext, hd0, s, cond))
 | IPushB (b0) -> (if continue_ cond then interp (ext, hd0, ((BVal (b0)) :: s), cond) else interp (ext, hd0, s, cond))
 | IObs (p0) -> (if continue_ cond then match Map.find p0 ext with 
Some (v0) -> (interp (ext, hd0, (v0 :: s), cond))
 | None  -> (Err (default_error)) else interp (ext, hd0, s, cond))
 | IIf  -> (if eqInt cond 0 then match s with 
[]  -> (Err (default_error))
 | s00 :: v0 -> (match s00 with 
BVal (b0) -> (interp (ext, hd0, v0, (bool_to_cond b0)))
 | ZVal (z0) -> (Err (default_error))) else interp (ext, hd0, s, (addInt 1 cond)))
 | IElse  -> (interp (ext, hd0, s, (flip cond)))
 | IEndIf  -> (interp (ext, hd0, s, (reset_decrement cond)))
 | IOp (op0) -> (if continue_ cond then match op0 with 
Add  -> (match s with 
[]  -> (Err (default_error))
 | l0 :: v0 -> (match l0 with 
BVal (b0) -> (Err (default_error))
 | ZVal (i0) -> (match v0 with 
[]  -> (Err (default_error))
 | s00 :: v00 -> (match s00 with 
BVal (b0) -> (Err (default_error))
 | ZVal (j0) -> (interp (ext, hd0, ((ZVal ((addInt i0 j0))) :: v00), cond))))))
 | Sub  -> (match s with 
[]  -> (Err (default_error))
 | l0 :: v0 -> (match l0 with 
BVal (b0) -> (Err (default_error))
 | ZVal (i0) -> (match v0 with 
[]  -> (Err (default_error))
 | s00 :: v00 -> (match s00 with 
BVal (b0) -> (Err (default_error))
 | ZVal (j0) -> (interp (ext, hd0, ((ZVal ((subInt i0 j0))) :: v00), cond))))))
 | Mult  -> (match s with 
[]  -> (Err (default_error))
 | l0 :: v0 -> (match l0 with 
BVal (b0) -> (Err (default_error))
 | ZVal (i0) -> (match v0 with 
[]  -> (Err (default_error))
 | s00 :: v00 -> (match s00 with 
BVal (b0) -> (Err (default_error))
 | ZVal (j0) -> (interp (ext, hd0, ((ZVal ((mulInt i0 j0))) :: v00), cond))))))
 | Lt  -> (match s with 
[]  -> (Err (default_error))
 | l0 :: v0 -> (match l0 with 
BVal (b0) -> (Err (default_error))
 | ZVal (i0) -> (match v0 with 
[]  -> (Err (default_error))
 | s00 :: v00 -> (match s00 with 
BVal (b0) -> (Err (default_error))
 | ZVal (j0) -> (interp (ext, hd0, ((BVal ((ltInt i0 j0))) :: v00), cond))))))
 | Le  -> (match s with 
[]  -> (Err (default_error))
 | l0 :: v0 -> (match l0 with 
BVal (b0) -> (Err (default_error))
 | ZVal (i0) -> (match v0 with 
[]  -> (Err (default_error))
 | s00 :: v00 -> (match s00 with 
BVal (b0) -> (Err (default_error))
 | ZVal (j0) -> (interp (ext, hd0, ((BVal ((leInt i0 j0))) :: v00), cond))))))
 | Equal  -> (match s with 
[]  -> (Err (default_error))
 | l0 :: v0 -> (match l0 with 
BVal (b0) -> (Err (default_error))
 | ZVal (i0) -> (match v0 with 
[]  -> (Err (default_error))
 | s00 :: v00 -> (match s00 with 
BVal (b0) -> (Err (default_error))
 | ZVal (j0) -> (interp (ext, hd0, ((BVal ((eqInt i0 j0))) :: v00), cond)))))) else interp (ext, hd0, s, cond)))
 in fun ext insts s cond -> interp (ext, insts, s, cond)

let receive (p : params) (s : storage) = let s0 = s in 
match interp p.(1) p.(0) [] 0 with 
Ok (v0) -> (Ok ( ([], v0)))
 | Err (e0) -> (Err (e0))

let%init storage (setup : unit) = 
let inner (ctx : simpleCallCtx) (setup : unit) = let ctx0 = ctx in 
let setup0 = setup in 
Ok ([]) in
let ctx = (Current.time (),
   (Current.sender (),
   (Current.amount (),
    Current.balance ()))) in
match (inner ctx setup) with Ok v -> v | Err e -> failwith e

let wrapper param (st : storage) = match receive param st with| Ok v -> v| Err e -> failwith e
let%entry main param st = wrapper param st
(*END*)
