
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
[@inline] let subTez (n : tez) (m : tez) = n - m
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
{ ctx_origin_ = Tezos.source;
  ctx_from_ = Tezos.sender;
  ctx_contract_address_ = Tezos.self_address;
  ctx_contract_balance_ = Tezos.balance;
  ctx_amount_ = Tezos.balance
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
chain_height_     = Tezos.level;
current_slot_     = Tezos.level;
finalized_height_ = Tezos.level;
}

(* chain projections as functions *)
let chain_height (c : chain ) = c.chain_height_
let current_slot (c : chain ) = c.current_slot_
let finalized_height (c : chain) = c.finalized_height_

type specif_sumbool = 
  Spec_left
| Spec_right


type counterRefinementTypes_storage = int

type counterRefinementTypes_msg = 
  Coun_Inc of int
| Coun_Dec of int


type 'a0 specif_sig = 
  Spec_exist of 'a0


let bool_bool_dec(b1 : bool) (b2 : bool) : specif_sumbool = (if b1 then fun (x : bool) -> if x then Spec_left else Spec_right else fun (x : bool) -> if x then Spec_right else Spec_left) b2

let counterRefinementTypes_Transaction_none : operation list = ([]: (operation) list)

let counterRefinementTypes_inc_counter(st : counterRefinementTypes_storage) (inc :  (int) specif_sig) :  (counterRefinementTypes_storage) specif_sig = Spec_exist ((addInt st (match inc with 
Spec_exist (a) -> a)))

let counterRefinementTypes_dec_counter(st : counterRefinementTypes_storage) (dec :  (int) specif_sig) :  (counterRefinementTypes_storage) specif_sig = Spec_exist ((subInt st (match dec with 
Spec_exist (a) -> a)))

let counterRefinementTypes_counter(msg : counterRefinementTypes_msg) (st : counterRefinementTypes_storage) :  ((operation list * counterRefinementTypes_storage)) option = match msg with 
Coun_Inc (i) -> (match bool_bool_dec true (ltInt 0 i) with 
Spec_left  -> (Some ( (counterRefinementTypes_Transaction_none, (match counterRefinementTypes_inc_counter st (Spec_exist (i)) with 
Spec_exist (a) -> a))))
 | Spec_right  -> (None: ((operation list * counterRefinementTypes_storage)) option))
 | Coun_Dec (i) -> (match bool_bool_dec true (ltInt 0 i) with 
Spec_left  -> (Some ( (counterRefinementTypes_Transaction_none, (match counterRefinementTypes_dec_counter st (Spec_exist (i)) with 
Spec_exist (a) -> a))))
 | Spec_right  -> (None: ((operation list * counterRefinementTypes_storage)) option))

let cameLIGOExtractionSetup_counter_wrapper(c : chain) (ctx : cctx) (s : counterRefinementTypes_storage) (m :  (counterRefinementTypes_msg) option) :  ((operation list * counterRefinementTypes_storage)) option = let c_ = c in 
let ctx_ = ctx in 
match m with 
Some (m0) -> (counterRefinementTypes_counter m0 s)
 | None  -> (None: ((operation list * counterRefinementTypes_storage)) option)

let init (setup : int) : counterRefinementTypes_storage = 

let inner (ctx : cctx) (setup : int) : (counterRefinementTypes_storage) option = 
let ctx_ = ctx in 
Some (setup) in
let ctx = cctx_instance in
match (inner ctx setup) with
  Some v -> v
| None -> (failwith (""): counterRefinementTypes_storage)
type init_args_ty = int
let init_wrapper (args : init_args_ty) =
  init args


type return = (operation) list * (counterRefinementTypes_storage option)
type parameter_wrapper =
  Init of init_args_ty
| Call of counterRefinementTypes_msg option

let wrapper (param, st : parameter_wrapper * (counterRefinementTypes_storage) option) : return =
  match param with  
    Init init_args -> (([]: operation list), Some (init init_args))
  | Call p -> (
    match st with
      Some st -> (match (cameLIGOExtractionSetup_counter_wrapper dummy_chain cctx_instance  st p) with   
                    Some v -> (v.0, Some v.1)
                  | None -> (failwith ("") : return))
    | None -> (failwith ("cannot call this endpoint before Init has been called"): return))

let main (action, st : parameter_wrapper * counterRefinementTypes_storage option) : return = wrapper (action, st)
