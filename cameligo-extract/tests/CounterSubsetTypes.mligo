
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
[@inline] let leTez (a : tez) (b : tez) = a <= b
[@inline] let ltTez (a : tez) (b : tez) = a < b
[@inline] let gtbTez (a : tez) (b : tez) = a > b
[@inline] let eqTez (a : tez) (b : tez) = a = b
[@inline] let natural_to_mutez (a: nat) : tez = a * 1mutez
[@inline] let divTez (a : tez) (b : tez) : tez = natural_to_mutez (a/b)
[@inline] let multTez (n : tez) (m : tez) = (n/1tez) * m
[@inline] let evenTez (i : tez) = (i mod 2n) = 0tez

[@inline] let addN (a : nat) (b : nat) = a + b
[@inline] let multN (a : nat) (b : nat) = a * b
[@inline] let modN (a : nat) (b : nat) = a mod b
[@inline] let divN (a : nat) (b : nat) = a / b
[@inline] let eqN (a : nat) (b : nat) = a = b
[@inline] let lebN (a : nat) (b : nat) = a <= b
[@inline] let ltbN (a : nat) (b : nat) = a < b
let divN_opt (n : nat) (m : nat) : nat option = match ediv n m with | Some (q,_) -> Some q | None -> None
let moduloN (n : nat) (m : nat) : nat = match ediv n m with | Some (_,r) -> r | None -> 0n
let subOption (n : nat) (m : nat) : nat option = if n < m then None else Some (abs (n-m))
let z_to_N (i : int) : nat = if i < 0 then 0n else abs i
let z_of_N (n : nat) : int = int (n)

[@inline] let andb (a : bool) (b : bool) = a && b
[@inline] let orb (a : bool) (b : bool) = a || b

[@inline] let eqb_time (a1 : timestamp) (a2 : timestamp) = a1 = a2
[@inline] let leb_time (a1 : timestamp) (a2 : timestamp) = a1 <= a2
[@inline] let ltb_time (a1 : timestamp) (a2 : timestamp) = a1 < a2

[@inline] let eq_addr (a1 : address) (a2 : address) = a1 = a2

type ('t,'e) result =
  Ok of 't
| Err of 'e

let get_contract_unit (a : address) : unit contract =
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
let chain_height (c : chain) = c.chain_height_
let current_slot (c : chain) = c.current_slot_
let finalized_height (c : chain) = c.finalized_height_

type counterRefinementTypes_storage = int

type counterRefinementTypes_Error = nat

type specif_sumbool = 
  Spec_left
| Spec_right


type counterRefinementTypes_msg = 
  Coun_Inc of int
| Coun_Dec of int


type 'a specif_sig = 
  Spec_exist of 'a


let bool_bool_dec (b1 : bool) (b2 : bool) : specif_sumbool = 
(if b1 then fun (x : bool) -> if x then Spec_left else Spec_right else fun (x : bool) -> if x then Spec_right else Spec_left) b2

let counterRefinementTypes_Transaction_none  : operation list = 
([]:operation list)

let counterRefinementTypes_inc_counter (st : counterRefinementTypes_storage) (inc : int specif_sig) : counterRefinementTypes_storage specif_sig = 
Spec_exist (addInt st (match inc with 
Spec_exist a0 -> a0))

let counterRefinementTypes_default_error  : counterRefinementTypes_Error = 
1n

let counterRefinementTypes_dec_counter (st : counterRefinementTypes_storage) (dec : int specif_sig) : counterRefinementTypes_storage specif_sig = 
Spec_exist (subInt st (match dec with 
Spec_exist a0 -> a0))

let counterRefinementTypes_counter (msg : counterRefinementTypes_msg) (st : counterRefinementTypes_storage) : ((operation list * counterRefinementTypes_storage), counterRefinementTypes_Error) result = 
match msg with 
Coun_Inc i0 -> (match bool_bool_dec true (ltInt 0 i0) with 
Spec_left  -> ((Ok (counterRefinementTypes_Transaction_none, (match counterRefinementTypes_inc_counter st (Spec_exist i0) with 
Spec_exist a0 -> a0))):((operation list * counterRefinementTypes_storage), counterRefinementTypes_Error) result)
 | Spec_right  -> ((Err counterRefinementTypes_default_error):((operation list * counterRefinementTypes_storage), counterRefinementTypes_Error) result))
 | Coun_Dec i0 -> (match bool_bool_dec true (ltInt 0 i0) with 
Spec_left  -> ((Ok (counterRefinementTypes_Transaction_none, (match counterRefinementTypes_dec_counter st (Spec_exist i0) with 
Spec_exist a0 -> a0))):((operation list * counterRefinementTypes_storage), counterRefinementTypes_Error) result)
 | Spec_right  -> ((Err counterRefinementTypes_default_error):((operation list * counterRefinementTypes_storage), counterRefinementTypes_Error) result))

let cameLIGOExtractionSetup_counter_wrapper (c : chain) (ctx : cctx) (s : counterRefinementTypes_storage) (m : counterRefinementTypes_msg option) : ((operation list * counterRefinementTypes_storage), counterRefinementTypes_Error) result = 
let c_ = c in 
let ctx_ = ctx in 
match m with 
Some m0 -> (counterRefinementTypes_counter m0 s)
 | None  -> ((Err counterRefinementTypes_default_error):((operation list * counterRefinementTypes_storage), counterRefinementTypes_Error) result)

let init (setup : int) : (counterRefinementTypes_storage, counterRefinementTypes_Error) result = let inner (setup : int) :(counterRefinementTypes_storage, counterRefinementTypes_Error) result = 
((Ok setup):(int, counterRefinementTypes_Error) result) in
match (inner setup) with
  Ok v -> Ok v
| Err e -> (failwith e: (counterRefinementTypes_storage, counterRefinementTypes_Error) result)


type return = (operation) list * counterRefinementTypes_storage

let main (p, st : counterRefinementTypes_msg option * counterRefinementTypes_storage) : return = 
   (match (cameLIGOExtractionSetup_counter_wrapper dummy_chain cctx_instance  st p) with 
      Ok v -> (v.0, v.1)
    | Err e -> (failwith e : return))
