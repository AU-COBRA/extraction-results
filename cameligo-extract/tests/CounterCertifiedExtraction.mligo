
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
  ctx_amount_ = Tezos.amount
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
let test_account : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)

type storage = (int * address)

type msg = 
  Inc of int
| Dec of int


let inc_balance (st : storage) (new_balance : int) : (int * address) = 
((addInt st.0 new_balance), st.1)

let dec_balance (st : storage) (new_balance : int) : (int * address) = 
((subInt st.0 new_balance), st.1)

let counter_inner (msg : msg) (st : storage) : (operation list * storage) option = 
match msg with 
Inc i -> (if leInt 0 i then Some (([]:operation list), (inc_balance st i)) else (None:(operation list * storage) option))
 | Dec i -> (if leInt 0 i then Some (([]:operation list), (dec_balance st i)) else (None:(operation list * storage) option))

let counter (c : chain) (ctx : cctx) (st : storage) (msg : msg option) : (operation list * storage) option = 
let c_ = c in 
let ctx_ = ctx in 
match msg with 
Some msg0 -> (counter_inner msg0 st)
 | None  -> (None:(operation list * storage) option)

let init (setup : (int * address)) : storage = 

let inner (ctx : cctx) (setup : (int * address)) :storage option = 
let ctx_ = ctx in 
Some setup in
let ctx = cctx_instance in
match (inner ctx setup) with
  Some v -> v
| None -> (failwith (""): storage)
type init_args_ty = (int * address)
let init_wrapper (args : init_args_ty) =
  init args


type return = (operation) list * (storage option)
type parameter_wrapper =
  Init of init_args_ty
| Call of msg option

let wrapper (param, st : parameter_wrapper * (storage) option) : return =
  match param with 
    Init init_args -> (
  match st with 
      Some st -> (failwith ("Cannot call Init twice"): return)
    | None -> (([]: operation list), Some (init init_args)))
  | Call p -> (
    match st with
      Some st -> (match (counter dummy_chain cctx_instance  st p) with   
                    Some v -> (v.0, Some v.1)
                  | None -> (failwith ("") : return))
    | None -> (failwith ("cannot call this endpoint before Init has been called"): return))
let main (action, st : parameter_wrapper * storage option) : return = wrapper (action, st)
