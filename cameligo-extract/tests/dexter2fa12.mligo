
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

let call_to_token (type msg) (addr : address) (amt : nat) (msg : msg) : operation =
  let token_ : msg contract =
  match (Tezos.get_contract_opt (addr) : msg contract option) with
    Some contract -> contract
  | None -> (failwith "Contract not found." : msg contract) in
  Tezos.transaction msg (natural_to_mutez amt) token_

[@inline] let mk_callback (type msg)(addr : address) (msg : msg) : operation = call_to_token addr 0n msg

[@inline] let natural_to_mutez (a: nat): tez = a * 1mutez

let subNTruncated (n : nat) (m : nat) : nat = if n < m then 0n else abs (n-m)

type dexter2FA12_Setup = 
  Dext_build_setup of (address * address * nat)


type dexter2FA12_State = 
  Dext_build_state of ((address, nat) map * ((address * address), nat) map * address * nat)


type dexter2FA12_Error = nat

type dexter2FA12_transfer_param = 
  Dext_build_transfer_param of (address * address * nat)


type dexter2FA12_approve_param = 
  Dext_build_approve_param of (address * nat)


type dexter2FA12_mintOrBurn_param = 
  Dext_build_mintOrBurn_param of (int * address)


type dexter2FA12_callback = 
  Dext_Build_callback of address


type dexter2FA12_getAllowance_param = 
  Dext_build_getAllowance_param of ((address * address) * dexter2FA12_callback)


type dexter2FA12_getBalance_param = 
  Dext_build_getBalance_param of (address * dexter2FA12_callback)


type dexter2FA12_getTotalSupply_param = 
  Dext_build_getTotalSupply_param of (unit * dexter2FA12_callback)


type dexter2FA12_Msg = 
  Dext_msg_transfer of dexter2FA12_transfer_param
| Dext_msg_approve of dexter2FA12_approve_param
| Dext_msg_mint_or_burn of dexter2FA12_mintOrBurn_param
| Dext_msg_get_allowance of dexter2FA12_getAllowance_param
| Dext_msg_get_balance of dexter2FA12_getBalance_param
| Dext_msg_get_total_supply of dexter2FA12_getTotalSupply_param


type 'a dexter2FA12_FA12ReceiverMsg = 
  Dext_receive_allowance of nat
| Dext_receive_balance_of of nat
| Dext_receive_total_supply of nat
| Dext_other_msg of 'a


let lqt_provider (s : dexter2FA12_Setup) : address = 
match s with 
Dext_build_setup (initial_pool0, lqt_provider0, admin_0) -> lqt_provider0

let initial_pool (s : dexter2FA12_Setup) : nat = 
match s with 
Dext_build_setup (initial_pool0, lqt_provider0, admin_0) -> admin_0

let admin_ (s : dexter2FA12_Setup) : address = 
match s with 
Dext_build_setup (initial_pool0, lqt_provider0, admin_0) -> initial_pool0

let throwIf(type e) (cond : bool) (err : e) : (unit, e) result = 
if cond then ((Err err):(unit, e) result) else ((Ok ()):(unit, e) result)

let without_actions(type t e) (x : (t, e) result) : ((t * operation list), e) result = 
match x with 
Ok t0 -> ((Ok (t0, ([]:operation list))):((t * operation list), e) result)
 | Err e0 -> ((Err e0):((t * operation list), e) result)

let allowances (s : dexter2FA12_State) : ((address * address), nat) map = 
match s with 
Dext_build_state (total_supply0, admin0, allowances0, tokens0) -> admin0

let tokens (s : dexter2FA12_State) : (address, nat) map = 
match s with 
Dext_build_state (total_supply0, admin0, allowances0, tokens0) -> total_supply0

let from (t : dexter2FA12_transfer_param) : address = 
match t with 
Dext_build_transfer_param (value0, to0, from0) -> value0

let value (t : dexter2FA12_transfer_param) : nat = 
match t with 
Dext_build_transfer_param (value0, to0, from0) -> from0

let maybe (n : nat) : nat option = 
if eqN n 0n then (None:nat option) else Some n

let to (t : dexter2FA12_transfer_param) : address = 
match t with 
Dext_build_transfer_param (value0, to0, from0) -> to0

let admin (s : dexter2FA12_State) : address = 
match s with 
Dext_build_state (total_supply0, admin0, allowances0, tokens0) -> allowances0

let total_supply (s : dexter2FA12_State) : nat = 
match s with 
Dext_build_state (total_supply0, admin0, allowances0, tokens0) -> tokens0

let set_State_allowances (f : ((address * address), nat) map -> ((address * address), nat) map) (r : dexter2FA12_State) : dexter2FA12_State = 
Dext_build_state ((tokens r), (f (allowances r)), (admin r), (total_supply r))

let setter_from_getter_State_allowances  : (((address * address), nat) map -> ((address * address), nat) map) -> dexter2FA12_State -> dexter2FA12_State = 
set_State_allowances

let set_State_tokens (f : (address, nat) map -> (address, nat) map) (r : dexter2FA12_State) : dexter2FA12_State = 
Dext_build_state ((f (tokens r)), (allowances r), (admin r), (total_supply r))

let setter_from_getter_State_tokens  : ((address, nat) map -> (address, nat) map) -> dexter2FA12_State -> dexter2FA12_State = 
set_State_tokens

let try_transfer (sender : address) (param : dexter2FA12_transfer_param) (state : dexter2FA12_State) : (dexter2FA12_State, dexter2FA12_Error) result = 
let allowances_ = allowances state in 
let tokens_ = tokens state in 
match if eq_addr sender (from param) then ((Ok allowances_):(((address * address), nat) map, dexter2FA12_Error) result) else let allowance_key = ((from param), sender) in 
let authorized_value = match Map.find_opt allowance_key allowances_ with 
Some v0 -> v0
 | None  -> 0n in 
match throwIf (ltbN authorized_value (value param)) 1n with 
Ok t0 -> ((Ok (Map.update allowance_key (maybe (subNTruncated authorized_value (value param))) allowances_)):(((address * address), nat) map, dexter2FA12_Error) result)
 | Err e0 -> ((Err e0):(((address * address), nat) map, dexter2FA12_Error) result) with 
Ok t0 -> (match let from_balance = match Map.find_opt (from param) tokens_ with 
Some v0 -> v0
 | None  -> 0n in 
match throwIf (ltbN from_balance (value param)) 1n with 
Ok t1 -> ((Ok (Map.update (from param) (maybe (subNTruncated from_balance (value param))) tokens_)):((address, nat) map, dexter2FA12_Error) result)
 | Err e0 -> ((Err e0):((address, nat) map, dexter2FA12_Error) result) with 
Ok t1 -> (let tokens_0 = let to_balance = match Map.find_opt (to param) t1 with 
Some v0 -> v0
 | None  -> 0n in 
Map.update (to param) (maybe (addN to_balance (value param))) t1 in 
((Ok (setter_from_getter_State_allowances (fun (a : ((address * address), nat) map) -> t0) (setter_from_getter_State_tokens (fun (a : (address, nat) map) -> tokens_0) state))):(dexter2FA12_State, dexter2FA12_Error) result))
 | Err e0 -> ((Err e0):(dexter2FA12_State, dexter2FA12_Error) result))
 | Err e0 -> ((Err e0):(dexter2FA12_State, dexter2FA12_Error) result)

let spender (a : dexter2FA12_approve_param) : address = 
match a with 
Dext_build_approve_param (value_0, spender0) -> value_0

let value_ (a : dexter2FA12_approve_param) : nat = 
match a with 
Dext_build_approve_param (value_0, spender0) -> spender0

let try_approve (sender : address) (param : dexter2FA12_approve_param) (state : dexter2FA12_State) : (dexter2FA12_State, dexter2FA12_Error) result = 
let allowances_ = allowances state in 
let allowance_key = (sender, (spender param)) in 
let previous_value = match Map.find_opt allowance_key allowances_ with 
Some v0 -> v0
 | None  -> 0n in 
match throwIf (andb (ltbN 0n previous_value) (ltbN 0n (value_ param))) 1n with 
Ok t0 -> (let allowances_0 = Map.update allowance_key (maybe (value_ param)) allowances_ in 
((Ok (setter_from_getter_State_allowances (fun (a : ((address * address), nat) map) -> allowances_0) state)):(dexter2FA12_State, dexter2FA12_Error) result))
 | Err e0 -> ((Err e0):(dexter2FA12_State, dexter2FA12_Error) result)

let target (m : dexter2FA12_mintOrBurn_param) : address = 
match m with 
Dext_build_mintOrBurn_param (target0, quantity0) -> quantity0

let quantity (m : dexter2FA12_mintOrBurn_param) : int = 
match m with 
Dext_build_mintOrBurn_param (target0, quantity0) -> target0

let set_State_total_supply (f : nat -> nat) (r : dexter2FA12_State) : dexter2FA12_State = 
Dext_build_state ((tokens r), (allowances r), (admin r), (f (total_supply r)))

let setter_from_getter_State_total_supply  : (nat -> nat) -> dexter2FA12_State -> dexter2FA12_State = 
set_State_total_supply

let try_mint_or_burn (sender : address) (param : dexter2FA12_mintOrBurn_param) (state : dexter2FA12_State) : (dexter2FA12_State, dexter2FA12_Error) result = 
match throwIf (not (eq_addr sender (admin state))) 1n with 
Ok t0 -> (let tokens_ = tokens state in 
let old_balance = match Map.find_opt (target param) tokens_ with 
Some v0 -> v0
 | None  -> 0n in 
let new_balance = addInt (z_of_N old_balance) (quantity param) in 
match throwIf (ltInt new_balance 0) 1n with 
Ok t1 -> (let tokens_0 = Map.update (target param) (maybe (abs new_balance)) tokens_ in 
let total_supply_ = abs (addInt (z_of_N (total_supply state)) (quantity param)) in 
((Ok (setter_from_getter_State_total_supply (fun (a : nat) -> total_supply_) (setter_from_getter_State_tokens (fun (a : (address, nat) map) -> tokens_0) state))):(dexter2FA12_State, dexter2FA12_Error) result))
 | Err e0 -> ((Err e0):(dexter2FA12_State, dexter2FA12_Error) result))
 | Err e0 -> ((Err e0):(dexter2FA12_State, dexter2FA12_Error) result)

let request (g : dexter2FA12_getAllowance_param) : (address * address) = 
match g with 
Dext_build_getAllowance_param (allowance_callback0, request0) -> allowance_callback0

let return_addr (c : dexter2FA12_callback) : address = 
match c with 
Dext_Build_callback return_addr0 -> return_addr0

let callback_addr (c : dexter2FA12_callback) : address = 
return_addr c

let allowance_callback (g : dexter2FA12_getAllowance_param) : dexter2FA12_callback = 
match g with 
Dext_build_getAllowance_param (allowance_callback0, request0) -> request0

let receive_allowance_ (n : nat) : unit dexter2FA12_FA12ReceiverMsg = 
Dext_receive_allowance n

let try_get_allowance (param : dexter2FA12_getAllowance_param) (state : dexter2FA12_State) : operation list = 
let value = match Map.find_opt (request param) (allowances state) with 
Some v0 -> v0
 | None  -> 0n in 
(mk_callback (callback_addr (allowance_callback param)) (receive_allowance_ value)) :: ([]:operation list)

let owner_ (g : dexter2FA12_getBalance_param) : address = 
match g with 
Dext_build_getBalance_param (balance_callback0, owner_0) -> balance_callback0

let balance_callback (g : dexter2FA12_getBalance_param) : dexter2FA12_callback = 
match g with 
Dext_build_getBalance_param (balance_callback0, owner_0) -> owner_0

let receive_balance_of_ (n : nat) : unit dexter2FA12_FA12ReceiverMsg = 
Dext_receive_balance_of n

let try_get_balance (param : dexter2FA12_getBalance_param) (state : dexter2FA12_State) : operation list = 
let value = match Map.find_opt (owner_ param) (tokens state) with 
Some v0 -> v0
 | None  -> 0n in 
(mk_callback (callback_addr (balance_callback param)) (receive_balance_of_ value)) :: ([]:operation list)

let supply_callback (g : dexter2FA12_getTotalSupply_param) : dexter2FA12_callback = 
match g with 
Dext_build_getTotalSupply_param (supply_callback0, request_0) -> request_0

let receive_total_supply_ (n : nat) : unit dexter2FA12_FA12ReceiverMsg = 
Dext_receive_total_supply n

let try_get_total_supply (param : dexter2FA12_getTotalSupply_param) (state : dexter2FA12_State) : operation list = 
let value = total_supply state in 
(mk_callback (callback_addr (supply_callback param)) (receive_total_supply_ value)) :: ([]:operation list)

let receive_lqt (ctx : cctx) (state : dexter2FA12_State) (maybe_msg : dexter2FA12_Msg option) : ((dexter2FA12_State * operation list), dexter2FA12_Error) result = 
let sender0 = ctx_from ctx in 
let without_statechange = fun (acts : operation list) -> ((Ok (state, acts)):((dexter2FA12_State * operation list), dexter2FA12_Error) result) in 
match throwIf ((fun (x : tez) -> 0tez < x) (ctx_amount ctx)) 1n with 
Ok t0 -> (match maybe_msg with 
Some m0 -> (match m0 with 
Dext_msg_transfer param0 -> (without_actions (try_transfer sender0 param0 state))
 | Dext_msg_approve param0 -> (without_actions (try_approve sender0 param0 state))
 | Dext_msg_mint_or_burn param0 -> (without_actions (try_mint_or_burn sender0 param0 state))
 | Dext_msg_get_allowance param0 -> (without_statechange (try_get_allowance param0 state))
 | Dext_msg_get_balance param0 -> (without_statechange (try_get_balance param0 state))
 | Dext_msg_get_total_supply param0 -> (without_statechange (try_get_total_supply param0 state)))
 | None  -> ((Err 1n):((dexter2FA12_State * operation list), dexter2FA12_Error) result))
 | Err e0 -> ((Err e0):((dexter2FA12_State * operation list), dexter2FA12_Error) result)

let receive_ (chain : chain) (ctx : cctx) (state : dexter2FA12_State) (maybe_msg : dexter2FA12_Msg option) : ((operation list * dexter2FA12_State), dexter2FA12_Error) result = 
match receive_lqt ctx state maybe_msg with 
Ok x0 -> ((Ok (x0.1, x0.0)):((operation list * dexter2FA12_State), dexter2FA12_Error) result)
 | Err e0 -> ((Err e0):((operation list * dexter2FA12_State), dexter2FA12_Error) result)

let init (setup : dexter2FA12_Setup) : (dexter2FA12_State, dexter2FA12_Error) result = let inner (setup : dexter2FA12_Setup) :(dexter2FA12_State, dexter2FA12_Error) result = 
((Ok (Dext_build_state ((Map.add (lqt_provider setup) (initial_pool setup) (Map.empty: (address, nat) map)), (Map.empty: ((address * address), nat) map), (admin_ setup), (initial_pool setup)))):(dexter2FA12_State, dexter2FA12_Error) result) in
match (inner setup) with
  Ok v -> Ok v
| Err e -> (failwith e: (dexter2FA12_State, dexter2FA12_Error) result)


type return = (operation) list * dexter2FA12_State

let main (p, st : dexter2FA12_Msg option * dexter2FA12_State) : return = 
   (match (receive_ dummy_chain cctx_instance  st p) with   
      Ok v -> (v.0, v.1)
    | Err e -> (failwith e : return))
