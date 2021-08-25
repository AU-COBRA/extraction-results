
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
let init_storage :  (timestamp * (tez * address)) =
          (Tezos.now, (42tez,("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address)))

type time_coq = 
  Time_coq of (nat)


type msg_coq = 
  Donate_coq
| GetFunds_coq
| Claim_coq


let leb_time (t1 : time_coq) (t2 : time_coq) = match t1 with 
Time_coq (n1) -> (match t2 with 
Time_coq (n2) -> (lebN n1 n2))

let update_contribs (f_st : ((time_coq * (tez * address)) * ((address,tez) map * bool))) (cs : (address,tez) map) =  (f_st.0,  (cs, f_st.1.1))

let ltb_time (t1 : time_coq) (t2 : time_coq) = match t1 with 
Time_coq (n1) -> (match t2 with 
Time_coq (n2) -> (ltbN n1 n2))

let maybe_bind_unit (o :  (unit) option) (b :  (( (operation) list * ((time_coq * (tez * address)) * ((address,tez) map * bool)))) option) = match o with 
Some (a) -> b
 | None  -> (None: (( (operation) list * ((time_coq * (tez * address)) * ((address,tez) map * bool)))) option)

let validate (tx_amount : tez) = if eqTez 0tez tx_amount then Some (()) else (None: (unit) option)

let set_done (f_st : ((time_coq * (tez * address)) * ((address,tez) map * bool))) =  (f_st.0,  (f_st.1.0, true))

let receive (m : msg_coq) (s : ((time_coq * (tez * address)) * ((address,tez) map * bool))) (ctx : (time_coq * (address * (tez * tez)))) = match m with 
Donate_coq  -> (if leb_time ctx.0 s.0.0 then match Map.find_opt ctx.1.0 s.1.0 with 
Some (v) -> (Some ( (([]: (operation) list), (update_contribs s (Map.add ctx.1.0 (addTez v ctx.1.1.0) s.1.0)))))
 | None  -> (Some ( (([]: (operation) list), (update_contribs s (Map.add ctx.1.0 ctx.1.1.0 s.1.0))))) else (None: (( (operation) list * ((time_coq * (tez * address)) * ((address,tez) map * bool)))) option))
 | GetFunds_coq  -> (if andb (andb (eq_addr s.0.1.1 ctx.1.0) (ltb_time s.0.0 ctx.0)) (leTez s.0.1.0 ctx.1.1.1) then maybe_bind_unit (validate ctx.1.1.0) (Some ( ((Tezos.transaction unit ctx.1.1.1 (get_contract_unit ctx.1.0) :: ([]: (operation) list)), (set_done s)))) else (None: (( (operation) list * ((time_coq * (tez * address)) * ((address,tez) map * bool)))) option))
 | Claim_coq  -> (if andb (andb (ltb_time s.0.0 ctx.0) (ltTez ctx.1.1.1 s.0.1.0)) (not s.1.1) then match Map.find_opt ctx.1.0 s.1.0 with 
Some (v) -> (maybe_bind_unit (validate ctx.1.1.0) (Some ( ((Tezos.transaction unit v (get_contract_unit ctx.1.0) :: ([]: (operation) list)), (update_contribs s (Map.add ctx.1.0 0tez s.1.0))))))
 | None  -> (None: (( (operation) list * ((time_coq * (tez * address)) * ((address,tez) map * bool)))) option) else (None: (( (operation) list * ((time_coq * (tez * address)) * ((address,tez) map * bool)))) option))

let crowdfunding_receive_inner (c : chain) (ctx : cctx) (params : msg_coq) (st : ((time_coq * (tez * address)) * ((address,tez) map * bool))) = receive params st  ((Time_coq (c.current_slot)),  (((fun (x : address) -> x) Tezos.sender),  (Tezos.amount, (ctx_contract_balance ctx))))

let crowdfunding_receive (c : chain) (ctx : cctx) (st : ((time_coq * (tez * address)) * ((address,tez) map * bool))) (msg :  (msg_coq) option) = match msg with 
Some (msg0) -> (crowdfunding_receive_inner c ctx msg0 st)
 | None  -> (None: (( (operation) list * ((time_coq * (tez * address)) * ((address,tez) map * bool)))) option)

let init (setup : (time_coq * (tez * address))) : ((time_coq * (tez * address)) * ((address,tez) map * bool)) = 

let inner (ctx : cctx) (setup : (time_coq * (tez * address))) : (((time_coq * (tez * address)) * ((address,tez) map * bool))) option = 
if eqTez Tezos.amount 0tez then Some ( (setup,  ((Map.empty : (address,tez) map), false))) else (None: (((time_coq * (tez * address)) * ((address,tez) map * bool))) option) in
let ctx = cctx_instance in
match (inner ctx setup) with
  Some v -> v
| None -> (failwith (""): ((time_coq * (tez * address)) * ((address,tez) map * bool)))
type init_args_ty = (time_coq * (tez * address))
let init_wrapper (args : init_args_ty) =
  init args


type storage = ((time_coq * (tez * address)) * ((address,tez) map * bool))type return = (operation) list * (storage option)
type parameter_wrapper =
  Init of init_args_ty
| Call of msg_coq option

let wrapper (param, st : parameter_wrapper * (storage) option) : return =
  match param with  
    Init init_args -> (([]: operation list), Some (init init_args))
  | Call p -> (
    match st with
      Some st -> (match (crowdfunding_receive dummy_chain cctx_instance  st p) with   
                    Some v -> (v.0, Some v.1)
                  | None -> (failwith ("") : return))
    | None -> (failwith ("cannot call this endpoint before Init has been called"): return))
let main (action, st : parameter_wrapper * storage option) : return = wrapper (action, st)
