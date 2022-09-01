
[@inline] let addInt (i : int) (j : int) = i + j
[@inline] let subInt (i : int) (j : int) = i - j
[@inline] let subIntTruncated (a : int) (b : int) = let res = a - b in if res < 0 then 0 else res
[@inline] let multInt (i : int) (j : int) = i * j
[@inline] let divInt (i : int) (j : int) = i / j
[@inline] let modInt (a : int)(b : int) : int = int (a mod b)
[@inline] let leInt (i : int) (j : int) = i <= j
[@inline] let ltInt (i : int) (j : int) = i < j
[@inline] let eqInt (i : int) (j : int) = i = j

[@inline] let addTez (n : tez) (m : tez) = n + m
[@inline] let subTez (n : tez) (m : tez) : tez option = n - m
[@inline] let leTez (a : tez ) (b : tez ) = a <= b
[@inline] let ltTez (a : tez ) (b : tez ) =  a < b
[@inline] let gtbTez (a : tez ) (b : tez ) =  a > b
[@inline] let eqTez (a : tez ) (b : tez ) = a = b
[@inline] let natural_to_mutez (a: nat): tez = a * 1mutez
[@inline] let divTez (a : tez) (b : tez) : tez = natural_to_mutez (a/b)
[@inline] let multTez (n : tez) (m : tez) = (n/1tez) * m
[@inline] let evenTez (i : tez) = (i mod 2n) = 0tez

[@inline] let addN (a : nat ) (b : nat ) = a + b
[@inline] let multN (a : nat ) (b : nat ) = a * b
[@inline] let modN (a : nat ) (b : nat ) = a mod b
[@inline] let divN (a : nat ) (b : nat ) = a / b
[@inline] let eqN (a : nat ) (b : nat ) = a = b
[@inline] let lebN (a : nat ) (b : nat ) = a <= b
[@inline] let ltbN (a : nat ) (b : nat ) = a < b
let divN_opt (n : nat) (m : nat) : nat option = match ediv n m with | Some (q,_) -> Some q | None -> None
let moduloN (n : nat) (m : nat) : nat = match ediv n m with | Some (_,r) -> r | None -> 0n
let subOption (n : nat) (m : nat) : nat option = if n < m then None else Some (abs (n-m))
let z_to_N (i : int) : nat = if i < 0 then 0n else abs i
let z_of_N (n : nat) : int = int (n)

[@inline] let andb (a : bool ) (b : bool ) = a && b
[@inline] let orb (a : bool ) (b : bool ) = a || b

[@inline] let eqb_time (a1 : timestamp) (a2 : timestamp) = a1 = a2
[@inline] let leb_time (a1 : timestamp) (a2 : timestamp) = a1 <= a2
[@inline] let ltb_time (a1 : timestamp) (a2 : timestamp) = a1 < a2

[@inline] let eq_addr (a1 : address) (a2 : address) = a1 = a2

type ('t,'e) result =
  Ok of 't
| Err of 'e

let get_contract_unit (a : address) : unit contract  =
  match (Tezos.get_contract_opt a : unit contract option) with
    Some c -> c
  | None -> (failwith ("Contract not found.") : unit contract)

(* ConCert's call context *)
type cctx = {
  ctx_origin_ : address;
  ctx_from_ : address;
  ctx_contract_address_ : address;
  ctx_contract_balance_ : tez;
  ctx_amount_ : tez
}
(* a call context instance with fields filled in with required data *)
let cctx_instance : cctx= 
{ ctx_origin_ = Tezos.get_source ();
  ctx_from_ = Tezos.get_sender ();
  ctx_contract_address_ = Tezos.get_self_address ();
  ctx_contract_balance_ = Tezos.get_balance ();
  ctx_amount_ = Tezos.get_amount ()
}

(* context projections as functions *)
let ctx_from (c : cctx) = c.ctx_from_
let ctx_origin (c : cctx) = c.ctx_origin_
let ctx_contract_address (c : cctx) = c.ctx_contract_address_
let ctx_contract_balance (c : cctx) = c.ctx_contract_balance_
let ctx_amount (c : cctx) = c.ctx_amount_
type chain = {
  chain_height_     : nat;
  current_slot_     : nat;
  finalized_height_ : nat;
}

let dummy_chain : chain = {
chain_height_     = Tezos.get_level ();
current_slot_     = Tezos.get_level ();
finalized_height_ = Tezos.get_level ();
}

(* chain projections as functions *)
let chain_height (c : chain ) = c.chain_height_
let current_slot (c : chain ) = c.current_slot_
let finalized_height (c : chain) = c.finalized_height_

type value = 
  BVal of bool
| ZVal of int


type storage = value list

type error = nat

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


type params = (instruction list * (string * int,value) map)

let continue_ (i : int) : bool = 
eqInt i 0

let default_error  : error = 
1n

let bool_to_cond (b : bool) : int = 
if b then 0 else 1

let flip (i : int) : int = 
if eqInt i 0 then 1 else if eqInt i 1 then 0 else i

let reset_decrement (i : int) : int = 
if leInt i 1 then 0 else subInt i 1

let interp  : (string * int,value) map -> instruction list -> storage -> int -> (storage, error) result = 
let rec interp (ext : (string * int,value) map) (insts : instruction list) (s : storage) (cond : int) : (storage, error) result = 
match insts with 
[]  -> ((Ok s):(storage, error) result)
 | inst00 :: hd0 -> (match inst00 with 
IPushZ i0 -> (if continue_ cond then interp ext hd0 ((ZVal i0) :: s) cond else interp ext hd0 s cond)
 | IPushB b0 -> (if continue_ cond then interp ext hd0 ((BVal b0) :: s) cond else interp ext hd0 s cond)
 | IObs p0 -> (if continue_ cond then match Map.find_opt p0 ext with 
Some v0 -> (interp ext hd0 (v0 :: s) cond)
 | None  -> ((Err default_error):(storage, error) result) else interp ext hd0 s cond)
 | IIf  -> (if eqInt cond 0 then match s with 
[]  -> ((Err default_error):(storage, error) result)
 | s00 :: v0 -> (match s00 with 
BVal b0 -> (interp ext hd0 v0 (bool_to_cond b0))
 | ZVal z0 -> ((Err default_error):(storage, error) result)) else interp ext hd0 s (addInt 1 cond))
 | IElse  -> (interp ext hd0 s (flip cond))
 | IEndIf  -> (interp ext hd0 s (reset_decrement cond))
 | IOp op0 -> (if continue_ cond then match op0 with 
Add  -> (match s with 
[]  -> ((Err default_error):(storage, error) result)
 | l0 :: v0 -> (match l0 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal i0 -> (match v0 with 
[]  -> ((Err default_error):(storage, error) result)
 | s00 :: v00 -> (match s00 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal j0 -> (interp ext hd0 ((ZVal (addInt i0 j0)) :: v00) cond)))))
 | Sub  -> (match s with 
[]  -> ((Err default_error):(storage, error) result)
 | l0 :: v0 -> (match l0 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal i0 -> (match v0 with 
[]  -> ((Err default_error):(storage, error) result)
 | s00 :: v00 -> (match s00 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal j0 -> (interp ext hd0 ((ZVal (subInt i0 j0)) :: v00) cond)))))
 | Mult  -> (match s with 
[]  -> ((Err default_error):(storage, error) result)
 | l0 :: v0 -> (match l0 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal i0 -> (match v0 with 
[]  -> ((Err default_error):(storage, error) result)
 | s00 :: v00 -> (match s00 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal j0 -> (interp ext hd0 ((ZVal (multInt i0 j0)) :: v00) cond)))))
 | Lt  -> (match s with 
[]  -> ((Err default_error):(storage, error) result)
 | l0 :: v0 -> (match l0 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal i0 -> (match v0 with 
[]  -> ((Err default_error):(storage, error) result)
 | s00 :: v00 -> (match s00 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal j0 -> (interp ext hd0 ((BVal (ltInt i0 j0)) :: v00) cond)))))
 | Le  -> (match s with 
[]  -> ((Err default_error):(storage, error) result)
 | l0 :: v0 -> (match l0 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal i0 -> (match v0 with 
[]  -> ((Err default_error):(storage, error) result)
 | s00 :: v00 -> (match s00 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal j0 -> (interp ext hd0 ((BVal (leInt i0 j0)) :: v00) cond)))))
 | Equal  -> (match s with 
[]  -> ((Err default_error):(storage, error) result)
 | l0 :: v0 -> (match l0 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal i0 -> (match v0 with 
[]  -> ((Err default_error):(storage, error) result)
 | s00 :: v00 -> (match s00 with 
BVal b0 -> ((Err default_error):(storage, error) result)
 | ZVal j0 -> (interp ext hd0 ((BVal (eqInt i0 j0)) :: v00) cond))))) else interp ext hd0 s cond))
 in (interp : (string * int,value) map -> instruction list -> storage -> int -> (storage, error) result)

let receive (p : params) (s : storage) : ((operation list * storage), error) result = 
let s0 = s in 
match interp p.1 p.0 ([]:value list) 0 with 
Ok v0 -> ((Ok (([]:operation list), v0)):((operation list * storage), error) result)
 | Err e0 -> ((Err e0):((operation list * storage), error) result)

let receive_ (c : chain) (ctx : cctx) (s : storage) (msg : params option) : ((operation list * storage), error) result = 
let c_ = c in 
let ctx_ = ctx in 
match msg with 
Some msg0 -> (receive msg0 s)
 | None  -> ((Err default_error):((operation list * storage), error) result)

let init (setup : unit) : (storage, error) result = let inner (setup : unit) :(storage, error) result = 
let setup0 = setup in 
((Ok ([]:value list)):(value list, error) result) in
match (inner setup) with
  Ok v -> Ok v
| Err e -> (failwith e: (storage, error) result)


type return = (operation) list * value list

let main (p, st : params option * value list) : return = 
   (match (receive_ dummy_chain cctx_instance  st p) with   
      Ok v -> (v.0, v.1)
    | Err e -> (failwith e : return))
