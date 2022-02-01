
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

let call_to_token (addr : address) (amt : nat) (msg : _msg) : operation =
  let token_ : _msg contract =
  match (Tezos.get_contract_opt (addr) : _msg contract option) with
    Some contract -> contract
  | None -> (failwith "Contract not found." : _msg contract) in
  Tezos.transaction msg (natural_to_mutez amt) token_

[@inline] let mk_callback (addr : address) (msg : _msg) : operation = call_to_token addr 0n msg

[@inline] let natural_to_mutez (a: nat): tez = a * 1mutez

[@inline] let mutez_to_natural (a: tez): nat = a / 1mutez

let xtz_transfer (to_ : address) (amount_ : nat) : operation option =
  match (Tezos.get_contract_opt to_ : unit contract option) with
    | None -> None
    | Some c -> Some (Tezos.transaction () (natural_to_mutez amount_) c)

let subNTruncated (n : nat) (m : nat) : nat = if n < m then 0n else abs (n-m)

type dexter2FA12_Setup = {
admin_ : address;
lqt_provider : address;
initial_pool : nat
}

type dexter2FA12_State = {
tokens :  (address, nat) map;
allowances :  ((address * address), nat) map;
admin : address;
total_supply : nat
}

type dexter2FA12_transfer_param = {
from : address;
to : address;
value : nat
}

type dexter2FA12_approve_param = {
spender : address;
value_ : nat
}

type dexter2FA12_mintOrBurn_param = {
quantity : int;
target : address
}

type dexter2FA12_callback = {
return_addr : address
}

type dexter2FA12_getAllowance_param = {
request : (address * address);
allowance_callback : dexter2FA12_callback
}

type dexter2FA12_getBalance_param = {
owner_ : address;
balance_callback : dexter2FA12_callback
}

type dexter2FA12_getTotalSupply_param = {
request_ : unit;
supply_callback : dexter2FA12_callback
}

type dexter2FA12_Msg = 
  Dext_msg_transfer of dexter2FA12_transfer_param
| Dext_msg_approve of dexter2FA12_approve_param
| Dext_msg_mint_or_burn of dexter2FA12_mintOrBurn_param
| Dext_msg_get_allowance of dexter2FA12_getAllowance_param
| Dext_msg_get_balance of dexter2FA12_getBalance_param
| Dext_msg_get_total_supply of dexter2FA12_getTotalSupply_param


type 'a0 dexter2FA12_FA12ReceiverMsg = 
  Dext_receive_allowance of nat
| Dext_receive_balance_of of nat
| Dext_receive_total_supply of nat
| Dext_other_msg of 'a0


let throwIf(cond : bool) :  (unit) option = if cond then (None: (unit) option) else Some (())

let maybe(n : nat) :  (nat) option = if eqN n 0n then (None: (nat) option) else Some (n)

let tokens(s : dexter2FA12_State) :  (address, nat) map = s.tokens

let allowances(s : dexter2FA12_State) :  ((address * address), nat) map = s.allowances

let admin(s : dexter2FA12_State) : address = s.admin

let total_supply(s : dexter2FA12_State) : nat = s.total_supply

let set_State_allowances(f :  ((address * address), nat) map ->  ((address * address), nat) map) (r : dexter2FA12_State) : dexter2FA12_State = ({tokens = (tokens r); allowances = (f (allowances r)); admin = (admin r); total_supply = (total_supply r)}: dexter2FA12_State)

let setter_from_getter_State_allowances : ( ((address * address), nat) map ->  ((address * address), nat) map) -> dexter2FA12_State -> dexter2FA12_State = set_State_allowances

let set_State_tokens(f :  (address, nat) map ->  (address, nat) map) (r : dexter2FA12_State) : dexter2FA12_State = ({tokens = (f (tokens r)); allowances = (allowances r); admin = (admin r); total_supply = (total_supply r)}: dexter2FA12_State)

let setter_from_getter_State_tokens : ( (address, nat) map ->  (address, nat) map) -> dexter2FA12_State -> dexter2FA12_State = set_State_tokens

let try_transfer(sender0 : address) (param : dexter2FA12_transfer_param) (state : dexter2FA12_State) :  (dexter2FA12_State) option = let allowances_ = state.allowances in 
let tokens_ = state.tokens in 
match if eq_addr sender0 param.from then Some (allowances_) else let allowance_key =  (param.from, sender0) in 
let authorized_value = match Map.find_opt allowance_key allowances_ with 
Some (v) -> v
 | None  -> 0n in 
match throwIf (ltbN authorized_value param.value) with 
Some (val0) -> (Some ((Map.update allowance_key (maybe (subNTruncated authorized_value param.value)) allowances_)))
 | None  -> (None: ( ((address * address), nat) map) option) with 
Some (val0) -> (match let from_balance = match Map.find_opt param.from tokens_ with 
Some (v) -> v
 | None  -> 0n in 
match throwIf (ltbN from_balance param.value) with 
Some (val1) -> (Some ((Map.update param.from (maybe (subNTruncated from_balance param.value)) tokens_)))
 | None  -> (None: ( (address, nat) map) option) with 
Some (val1) -> (let tokens_0 = let to_balance = match Map.find_opt param.to val1 with 
Some (v) -> v
 | None  -> 0n in 
Map.update param.to (maybe (addN to_balance param.value)) val1 in 
Some ((setter_from_getter_State_allowances (fun (a :  ((address * address), nat) map) -> val0) (setter_from_getter_State_tokens (fun (a :  (address, nat) map) -> tokens_0) state))))
 | None  -> (None: (dexter2FA12_State) option))
 | None  -> (None: (dexter2FA12_State) option)

let try_approve(sender0 : address) (param : dexter2FA12_approve_param) (state : dexter2FA12_State) :  (dexter2FA12_State) option = let allowances_ = state.allowances in 
let allowance_key =  (sender0, param.spender) in 
let previous_value = match Map.find_opt allowance_key allowances_ with 
Some (v) -> v
 | None  -> 0n in 
match throwIf (andb (ltbN 0n previous_value) (ltbN 0n param.value_)) with 
Some (val0) -> (let allowances_0 = Map.update allowance_key (maybe param.value_) allowances_ in 
Some ((setter_from_getter_State_allowances (fun (a :  ((address * address), nat) map) -> allowances_0) state)))
 | None  -> (None: (dexter2FA12_State) option)

let set_State_total_supply(f : nat -> nat) (r : dexter2FA12_State) : dexter2FA12_State = ({tokens = (tokens r); allowances = (allowances r); admin = (admin r); total_supply = (f (total_supply r))}: dexter2FA12_State)

let setter_from_getter_State_total_supply : (nat -> nat) -> dexter2FA12_State -> dexter2FA12_State = set_State_total_supply

let try_mint_or_burn(sender0 : address) (param : dexter2FA12_mintOrBurn_param) (state : dexter2FA12_State) :  (dexter2FA12_State) option = match throwIf (not (eq_addr sender0 state.admin)) with 
Some (val0) -> (let tokens_ = state.tokens in 
let old_balance = match Map.find_opt param.target tokens_ with 
Some (v) -> v
 | None  -> 0n in 
let new_balance = addInt (z_of_N old_balance) param.quantity in 
match throwIf (ltInt new_balance 0) with 
Some (val1) -> (let tokens_0 = Map.update param.target (maybe (abs new_balance)) tokens_ in 
let total_supply_ = abs (addInt (z_of_N state.total_supply) param.quantity) in 
Some ((setter_from_getter_State_total_supply (fun (a : nat) -> total_supply_) (setter_from_getter_State_tokens (fun (a :  (address, nat) map) -> tokens_0) state))))
 | None  -> (None: (dexter2FA12_State) option))
 | None  -> (None: (dexter2FA12_State) option)

let callback_addr(c : dexter2FA12_callback) : address = c.return_addr

let receive_allowance_(n : nat) :  (unit) dexter2FA12_FA12ReceiverMsg = Dext_receive_allowance (n)

let try_get_allowance(param : dexter2FA12_getAllowance_param) (state : dexter2FA12_State) :  (operation) list = let value = match Map.find_opt param.request state.allowances with 
Some (v) -> v
 | None  -> 0n in 
(mk_callback (callback_addr param.allowance_callback) (receive_allowance_ value)) :: ([]: (operation) list)

let receive_balance_of_(n : nat) :  (unit) dexter2FA12_FA12ReceiverMsg = Dext_receive_balance_of (n)

let try_get_balance(param : dexter2FA12_getBalance_param) (state : dexter2FA12_State) :  (operation) list = let value = match Map.find_opt param.owner_ state.tokens with 
Some (v) -> v
 | None  -> 0n in 
(mk_callback (callback_addr param.balance_callback) (receive_balance_of_ value)) :: ([]: (operation) list)

let receive_total_supply_(n : nat) :  (unit) dexter2FA12_FA12ReceiverMsg = Dext_receive_total_supply (n)

let try_get_total_supply(param : dexter2FA12_getTotalSupply_param) (state : dexter2FA12_State) :  (operation) list = let value = state.total_supply in 
(mk_callback (callback_addr param.supply_callback) (receive_total_supply_ value)) :: ([]: (operation) list)

let receive(ctx : cctx) (state : dexter2FA12_State) (maybe_msg :  (dexter2FA12_Msg) option) :  ((dexter2FA12_State *  (operation) list)) option = let sender0 = ctx_from ctx in 
let without_actions = fun (o :  (dexter2FA12_State) option) -> match o with 
Some (a) -> (Some ( (a, ([]: (operation) list))))
 | None  -> (None: ((dexter2FA12_State *  (operation) list)) option) in 
let without_statechange = fun (acts :  (operation) list) -> Some ( (state, acts)) in 
match throwIf ((fun (x : tez) -> 0tez < x) (ctx_amount ctx)) with 
Some (val0) -> (match maybe_msg with 
Some (m) -> (match m with 
Dext_msg_transfer (param) -> (without_actions (try_transfer sender0 param state))
 | Dext_msg_approve (param) -> (without_actions (try_approve sender0 param state))
 | Dext_msg_mint_or_burn (param) -> (without_actions (try_mint_or_burn sender0 param state))
 | Dext_msg_get_allowance (param) -> (without_statechange (try_get_allowance param state))
 | Dext_msg_get_balance (param) -> (without_statechange (try_get_balance param state))
 | Dext_msg_get_total_supply (param) -> (without_statechange (try_get_total_supply param state)))
 | None  -> (None: ((dexter2FA12_State *  (operation) list)) option))
 | None  -> (None: ((dexter2FA12_State *  (operation) list)) option)

let receive_(chain : chain) (ctx : cctx) (state : dexter2FA12_State) (maybe_msg :  (dexter2FA12_Msg) option) :  (( (operation) list * dexter2FA12_State)) option = match receive ctx state maybe_msg with 
Some (x) -> (Some ( (x.1, x.0)))
 | None  -> (None: (( (operation) list * dexter2FA12_State)) option)

let init (setup : dexter2FA12_Setup) : dexter2FA12_State = 

let inner (ctx : cctx) (setup : dexter2FA12_Setup) : (dexter2FA12_State) option = 
let ctx_ = ctx in 
Some (({tokens = (Map.add setup.lqt_provider setup.initial_pool (Map.empty:  (address, nat) map)); allowances = (Map.empty:  ((address * address), nat) map); admin = setup.admin_; total_supply = setup.initial_pool}: dexter2FA12_State)) in
let ctx = cctx_instance in
match (inner ctx setup) with
  Some v -> v
| None -> (failwith (""): dexter2FA12_State)
type init_args_ty = dexter2FA12_Setup
let init_wrapper (args : init_args_ty) =
  init args


type return = (operation) list * (dexter2FA12_State option)
type parameter_wrapper =
  Init of init_args_ty
| Call of dexter2FA12_Msg option

let wrapper (param, st : parameter_wrapper * (dexter2FA12_State) option) : return =
  match param with  
    Init init_args -> (([]: operation list), Some (init init_args))
  | Call p -> (
    match st with
      Some st -> (match (receive_ dummy_chain cctx_instance  st p) with   
                    Some v -> (v.0, Some v.1)
                  | None -> (failwith ("") : return))
    | None -> (failwith ("cannot call this endpoint before Init has been called"): return))

let main (action, st : parameter_wrapper * dexter2FA12_State option) : return = wrapper (action, st)
