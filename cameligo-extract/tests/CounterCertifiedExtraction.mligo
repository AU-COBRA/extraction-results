
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
  ctx_from_ : address;
  ctx_contract_address_ : address;
  ctx_contract_balance_ : tez;
  ctx_amount_ : tez
}
(* a call context instance with fields filled in with required data *)
let cctx_instance : cctx= 
{ ctx_from_ = Tezos.sender;
  ctx_contract_address_ = Tezos.self_address;
  ctx_contract_balance_ = Tezos.balance;
  ctx_amount_ = Tezos.balance
}

(* projections as functions *)
let ctx_from (c : cctx) = c.ctx_from_
let ctx_contract_address (c : cctx) = c.ctx_contract_address_
let ctx_contract_balance (c : cctx) = c.ctx_contract_balance_
let ctx_amount_ (c : cctx) = c.ctx_amount_
type chain = {
  chain_height     : nat;
  current_slot     : nat;
  finalized_height : nat;
}

let dummy_chain : chain = {
chain_height     = Tezos.level;
current_slot     = Tezos.level;
finalized_height = Tezos.level;
}
type chain = {
  chain_height     : nat;
  current_slot     : nat;
  finalized_height : nat;
}

let dummy_chain : chain = {
chain_height     = Tezos.level;
current_slot     = Tezos.level;
finalized_height = Tezos.level;
}
let test_account : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)

type storage = (tez * address)

type msg = 
  Inc of (tez)
| Dec of (tez)


let inc_balance (st : storage) (new_balance : tez) =  ((addTez st.0 new_balance), st.1)

let dec_balance (st : storage) (new_balance : tez) =  ((subTez st.0 new_balance), st.1)

let counter_inner (msg : msg) (st : storage) = match msg with 
Inc (i) -> (if leTez 0tez i then Some ( (([]: (operation) list), (inc_balance st i))) else (None: (( (operation) list * storage)) option))
 | Dec (i) -> (if leTez 0tez i then Some ( (([]: (operation) list), (dec_balance st i))) else (None: (( (operation) list * storage)) option))

let counter (c : chain) (ctx : cctx) (st : storage) (msg :  (msg) option) = let c_ = c in 
let ctx_ = ctx in 
match msg with 
Some (msg0) -> (counter_inner msg0 st)
 | None  -> (None: (( (operation) list * storage)) option)

let init (setup : (tez * address)) : storage = 

let inner (ctx : cctx) (setup : (tez * address)) : (storage) option = 
let ctx_ = ctx in 
Some (setup) in
let ctx = cctx_instance in
match (inner ctx setup) with
  Some v -> v
| None -> (failwith (""): storage)
type init_args_ty = (tez * address)
let init_wrapper (args : init_args_ty) =
  init args


type return = (operation) list * (storage option)
type parameter_wrapper =
  Init of init_args_ty
| Call of msg option

let wrapper (param, st : parameter_wrapper * (storage) option) : return =
  match param with  
    Init init_args -> (([]: operation list), Some (init init_args))
  | Call p -> (
    match st with
      Some st -> (match (counter dummy_chain cctx_instance  st p) with   
                    Some v -> (v.0, Some v.1)
                  | None -> (failwith ("") : return))
    | None -> (failwith ("cannot call this endpoint before Init has been called"): return))
let main (action, st : parameter_wrapper * storage option) : return = wrapper (action, st)
