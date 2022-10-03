
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
[@inline] let ltTez (a : tez ) (b : tez ) = a < b
[@inline] let gtbTez (a : tez ) (b : tez ) = a > b
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
let chain_height (c : chain ) = c.chain_height_
let current_slot (c : chain ) = c.current_slot_
let finalized_height (c : chain) = c.finalized_height_

let call_to_token (type msg) (addr : address) (amt : nat) (msg : msg) : operation =
  let token_ : msg contract =
  match (Tezos.get_contract_opt (addr) : msg contract option) with
    Some contract -> contract
  | None -> (failwith "Contract not found." : msg contract) in
  Tezos.transaction msg (natural_to_mutez amt) token_

[@inline] let natural_to_mutez (a: nat): tez = a * 1mutez

[@inline] let mutez_to_natural (a: tez): nat = a / 1mutez

let xtz_transfer (to_ : address) (amount_ : nat) : (operation, nat) result =
  match (Tezos.get_contract_opt to_ : unit contract option) with
    | None -> Err 0n
    | Some c -> Ok (Tezos.transaction () (natural_to_mutez amount_) c)

let subNTruncated (n : nat) (m : nat) : nat = if n < m then 0n else abs (n-m)

let divN_res (n : nat) (m : nat) : (nat, nat) result = match ediv n m with | Some (q,_) -> Ok q | None -> Err 0n

type fA2LegacyInterface_token_id = nat

type dexter2CPMM_Setup = {
lqtTotal_ : nat;
manager_ : address;
tokenAddress_ : address;
tokenId_ : nat
}

type dexter2CPMM_State = {
tokenPool : nat;
xtzPool : nat;
lqtTotal : nat;
selfIsUpdatingTokenPool : bool;
freezeBaker : bool;
manager : address;
tokenAddress : address;
tokenId : nat;
lqtAddress : address
}

type dexter2CPMM_Error = nat

type fA2LegacyInterface_balance_of_request = {
owner : address;
bal_req_token_id : fA2LegacyInterface_token_id
}

type fA2LegacyInterface_balance_of_response = {
request : fA2LegacyInterface_balance_of_request;
balance : nat
}

type fA2LegacyInterface_total_supply_response = {
supply_resp_token_id : fA2LegacyInterface_token_id;
total_supply : nat
}

type fA2LegacyInterface_token_metadata = {
metadata_token_id : fA2LegacyInterface_token_id;
metadata_decimals : nat
}

type fA2LegacyInterface_operator_tokens = 
  FA2L_all_tokens
| FA2L_some_tokens of fA2LegacyInterface_token_id list


type fA2LegacyInterface_operator_param = {
op_param_owner : address;
op_param_operator : address;
op_param_tokens : fA2LegacyInterface_operator_tokens
}

type fA2LegacyInterface_is_operator_response = {
operator : fA2LegacyInterface_operator_param;
is_operator : bool
}

type fA2LegacyInterface_self_transfer_policy = 
  FA2L_self_transfer_permitted
| FA2L_self_transfer_denied


type fA2LegacyInterface_operator_transfer_policy = 
  FA2L_operator_transfer_permitte
| FA2L_operator_transfer_denied


type fA2LegacyInterface_owner_transfer_policy = 
  FA2L_owner_no_op
| FA2L_optional_owner_hook
| FA2L_required_owner_hook


type fA2LegacyInterface_permissions_descriptor = {
descr_self : fA2LegacyInterface_self_transfer_policy;
descr_operator : fA2LegacyInterface_operator_transfer_policy;
descr_receiver : fA2LegacyInterface_owner_transfer_policy;
descr_sender : fA2LegacyInterface_owner_transfer_policy;
descr_custom : address option
}

type 'a fA2Token_FA2ReceiverMsg = 
  FA2T_receive_balance_of_param of fA2LegacyInterface_balance_of_response list
| FA2T_receive_total_supply_param of fA2LegacyInterface_total_supply_response list
| FA2T_receive_metadata_callback of fA2LegacyInterface_token_metadata list
| FA2T_receive_is_operator of fA2LegacyInterface_is_operator_response
| FA2T_receive_permissions_descri of fA2LegacyInterface_permissions_descriptor
| FA2T_other_msg of 'a


type dexter2CPMM_add_liquidity_param = {
owner : address;
minLqtMinted : nat;
maxTokensDeposited : nat;
add_deadline : nat
}

type dexter2CPMM_remove_liquidity_param = {
liquidity_to : address;
lqtBurned : nat;
minXtzWithdrawn : nat;
minTokensWithdrawn : nat;
remove_deadline : nat
}

type dexter2CPMM_xtz_to_token_param = {
tokens_to : address;
minTokensBought : nat;
xtt_deadline : nat
}

type dexter2CPMM_token_to_xtz_param = {
xtz_to : address;
tokensSold : nat;
minXtzBought : nat;
ttx_deadline : nat
}

type dexter2CPMM_set_baker_param = {
baker : key_hash option;
freezeBaker_ : bool
}

type dexter2CPMM_token_to_token_param = {
outputDexterContract : address;
to_ : address;
minTokensBought_ : nat;
tokensSold_ : nat;
ttt_deadline : nat
}

type dexter2CPMM_DexterMsg = 
  Dext_AddLiquidity of dexter2CPMM_add_liquidity_param
| Dext_RemoveLiquidity of dexter2CPMM_remove_liquidity_param
| Dext_XtzToToken of dexter2CPMM_xtz_to_token_param
| Dext_TokenToXtz of dexter2CPMM_token_to_xtz_param
| Dext_SetBaker of dexter2CPMM_set_baker_param
| Dext_SetManager of address
| Dext_SetLqtAddress of address
| Dext_UpdateTokenPool
| Dext_TokenToToken of dexter2CPMM_token_to_token_param


type dexter2CPMM_Msg = dexter2CPMM_DexterMsg fA2Token_FA2ReceiverMsg

type dEX2Extract_Result = ((dexter2CPMM_State * operation list), dexter2CPMM_Error) result

type dexter2CPMM_update_token_pool_internal_ = fA2LegacyInterface_balance_of_response list

type fA2LegacyInterface_transfer_destination = {
to_ : address;
dst_token_id : nat;
amount : nat
}

type fA2LegacyInterface_transfer = {
from_ : address;
txs : fA2LegacyInterface_transfer_destination list;
sender_callback_addr : address option
}

type fA2LegacyInterface_set_hook_param = {
hook_addr : address;
hook_permissions_descriptor : fA2LegacyInterface_permissions_descriptor
}

type fA2LegacyInterface_transfer_destination_descriptor = {
transfer_dst_descr_to_ : address option;
transfer_dst_descr_token_id : fA2LegacyInterface_token_id;
transfer_dst_descr_amount : nat
}

type fA2LegacyInterface_transfer_descriptor = {
transfer_descr_from_ : address option;
transfer_descr_txs : fA2LegacyInterface_transfer_destination_descriptor list
}

type fA2LegacyInterface_transfer_descriptor_param = {
transfer_descr_fa2 : address;
transfer_descr_batch : fA2LegacyInterface_transfer_descriptor list;
transfer_descr_operator : address
}

type 'a fA2LegacyInterface_callback = {
blob : 'a option;
return_addr : address
}

type fA2LegacyInterface_balance_of_param = {
bal_requests : fA2LegacyInterface_balance_of_request list;
bal_callback : fA2LegacyInterface_balance_of_response list fA2LegacyInterface_callback
}

type fA2LegacyInterface_total_supply_param = {
supply_param_token_ids : fA2LegacyInterface_token_id list;
supply_param_callback : fA2LegacyInterface_total_supply_response list fA2LegacyInterface_callback
}

type fA2LegacyInterface_token_metadata_param = {
metadata_token_ids : fA2LegacyInterface_token_id list;
metadata_callback : fA2LegacyInterface_token_metadata list fA2LegacyInterface_callback
}

type fA2LegacyInterface_update_operator = 
  FA2L_add_operator of fA2LegacyInterface_operator_param
| FA2L_remove_operator of fA2LegacyInterface_operator_param


type fA2LegacyInterface_is_operator_param = {
is_operator_operator : fA2LegacyInterface_operator_param;
is_operator_callback : fA2LegacyInterface_is_operator_response fA2LegacyInterface_callback
}

type fA2Token_Msg = 
  FA2T_msg_transfer of fA2LegacyInterface_transfer list
| FA2T_msg_set_transfer_hook of fA2LegacyInterface_set_hook_param
| FA2T_msg_receive_hook_transfer of fA2LegacyInterface_transfer_descriptor_param
| FA2T_msg_balance_of of fA2LegacyInterface_balance_of_param
| FA2T_msg_total_supply of fA2LegacyInterface_total_supply_param
| FA2T_msg_token_metadata of fA2LegacyInterface_token_metadata_param
| FA2T_msg_permissions_descriptor of fA2LegacyInterface_permissions_descriptor fA2LegacyInterface_callback
| FA2T_msg_update_operators of fA2LegacyInterface_update_operator list
| FA2T_msg_is_operator of fA2LegacyInterface_is_operator_param
| FA2T_msg_create_tokens of fA2LegacyInterface_token_id


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


let throwIf(type e) (cond : bool) (err : e) : (unit, e) result = 
if cond then ((Err err):(unit, e) result) else ((Ok ()):(unit, e) result)

let tokenPool (s : dexter2CPMM_State) : nat = 
s.tokenPool

let xtzPool (s : dexter2CPMM_State) : nat = 
s.xtzPool

let lqtTotal (s : dexter2CPMM_State) : nat = 
s.lqtTotal

let selfIsUpdatingTokenPool (s : dexter2CPMM_State) : bool = 
s.selfIsUpdatingTokenPool

let freezeBaker (s : dexter2CPMM_State) : bool = 
s.freezeBaker

let manager (s : dexter2CPMM_State) : address = 
s.manager

let tokenAddress (s : dexter2CPMM_State) : address = 
s.tokenAddress

let tokenId (s : dexter2CPMM_State) : nat = 
s.tokenId

let lqtAddress (s : dexter2CPMM_State) : address = 
s.lqtAddress

let set_State_selfIsUpdatingTokenPool (f : bool -> bool) (r : dexter2CPMM_State) : dexter2CPMM_State = 
({tokenPool = (tokenPool r); xtzPool = (xtzPool r); lqtTotal = (lqtTotal r); selfIsUpdatingTokenPool = (f (selfIsUpdatingTokenPool r)); freezeBaker = (freezeBaker r); manager = (manager r); tokenAddress = (tokenAddress r); tokenId = (tokenId r); lqtAddress = (lqtAddress r)}: dexter2CPMM_State)

let set_State_tokenPool (f : nat -> nat) (r : dexter2CPMM_State) : dexter2CPMM_State = 
({tokenPool = (f (tokenPool r)); xtzPool = (xtzPool r); lqtTotal = (lqtTotal r); selfIsUpdatingTokenPool = (selfIsUpdatingTokenPool r); freezeBaker = (freezeBaker r); manager = (manager r); tokenAddress = (tokenAddress r); tokenId = (tokenId r); lqtAddress = (lqtAddress r)}: dexter2CPMM_State)

let update_token_pool_internal (ctx : cctx) (state : dexter2CPMM_State) (token_pool : dexter2CPMM_update_token_pool_internal_) : dEX2Extract_Result = 
match throwIf (orb (not state.selfIsUpdatingTokenPool) (not (eq_addr (ctx_from ctx) state.tokenAddress))) 1n with 
Ok t0 -> (match throwIf ((fun (x : tez) -> 0tez < x) (ctx_amount ctx)) 1n with 
Ok t1 -> (match match token_pool with 
[]  -> ((Err 1n):(nat, dexter2CPMM_Error) result)
 | xs0 :: x0 -> ((Ok xs0.balance):(nat, dexter2CPMM_Error) result) with 
Ok t2 -> (let new_state = set_State_selfIsUpdatingTokenPool (fun (a : bool) -> false) (set_State_tokenPool (fun (a : nat) -> t2) state) in 
((Ok (new_state, ([]:operation list))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let ceildiv (n : nat) (m : nat) : (nat, dexter2CPMM_Error) result = 
if eqN (moduloN n m) 0n then divN_res n m else match divN_res n m with 
Ok t0 -> ((Ok (addN t0 1n)):(nat, dexter2CPMM_Error) result)
 | Err e0 -> ((Err e0):(nat, dexter2CPMM_Error) result)

let set_State_xtzPool (f : nat -> nat) (r : dexter2CPMM_State) : dexter2CPMM_State = 
({tokenPool = (tokenPool r); xtzPool = (f (xtzPool r)); lqtTotal = (lqtTotal r); selfIsUpdatingTokenPool = (selfIsUpdatingTokenPool r); freezeBaker = (freezeBaker r); manager = (manager r); tokenAddress = (tokenAddress r); tokenId = (tokenId r); lqtAddress = (lqtAddress r)}: dexter2CPMM_State)

let set_State_lqtTotal (f : nat -> nat) (r : dexter2CPMM_State) : dexter2CPMM_State = 
({tokenPool = (tokenPool r); xtzPool = (xtzPool r); lqtTotal = (f (lqtTotal r)); selfIsUpdatingTokenPool = (selfIsUpdatingTokenPool r); freezeBaker = (freezeBaker r); manager = (manager r); tokenAddress = (tokenAddress r); tokenId = (tokenId r); lqtAddress = (lqtAddress r)}: dexter2CPMM_State)

let token_transfer (state : dexter2CPMM_State) (from : address) (to0 : address) (amount0 : nat) : operation = 
call_to_token state.tokenAddress 0n (FA2T_msg_transfer (({from_ = from; txs = (({to_ = to0; dst_token_id = state.tokenId; amount = amount0}: fA2LegacyInterface_transfer_destination) :: ([]:fA2LegacyInterface_transfer_destination list)); sender_callback_addr = (None:address option)}: fA2LegacyInterface_transfer) :: ([]:fA2LegacyInterface_transfer list)))

let mint_or_burn (state : dexter2CPMM_State) (target : address) (quantitiy : int) : (operation, dexter2CPMM_Error) result = 
match throwIf (eq_addr state.lqtAddress ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address)) 1n with 
Ok t0 -> ((Ok (call_to_token state.lqtAddress 0n (Dext_msg_mint_or_burn (Dext_build_mintOrBurn_param (quantitiy, target))))):(operation, dexter2CPMM_Error) result)
 | Err e0 -> ((Err e0):(operation, dexter2CPMM_Error) result)

let add_liquidity (chain : chain) (ctx : cctx) (state : dexter2CPMM_State) (param : dexter2CPMM_add_liquidity_param) : dEX2Extract_Result = 
match throwIf state.selfIsUpdatingTokenPool 1n with 
Ok t0 -> (match throwIf (lebN param.add_deadline (current_slot chain)) 1n with 
Ok t1 -> (match divN_res (multN (mutez_to_natural (ctx_amount ctx)) state.lqtTotal) state.xtzPool with 
Ok t2 -> (match ceildiv (multN (mutez_to_natural (ctx_amount ctx)) state.tokenPool) state.xtzPool with 
Ok t3 -> (match throwIf (ltbN param.maxTokensDeposited t3) 1n with 
Ok t4 -> (match throwIf (ltbN t2 param.minLqtMinted) 1n with 
Ok t5 -> (let new_state = set_State_xtzPool (fun (a : nat) -> addN state.xtzPool (mutez_to_natural (ctx_amount ctx))) (set_State_tokenPool (fun (a : nat) -> addN state.tokenPool t3) (set_State_lqtTotal (fun (a : nat) -> addN state.lqtTotal t2) state)) in 
let op_token = token_transfer state (ctx_from ctx) (ctx_contract_address ctx) t3 in 
match mint_or_burn state param.owner (z_of_N t2) with 
Ok t6 -> ((Ok (new_state, (op_token :: (t6 :: ([]:operation list))))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let sub (n : nat) (m : nat) : (nat, dexter2CPMM_Error) result = 
match throwIf (ltbN n m) 1n with 
Ok t0 -> ((Ok (subNTruncated n m)):(nat, dexter2CPMM_Error) result)
 | Err e0 -> ((Err e0):(nat, dexter2CPMM_Error) result)

let remove_liquidity (chain : chain) (ctx : cctx) (state : dexter2CPMM_State) (param : dexter2CPMM_remove_liquidity_param) : dEX2Extract_Result = 
match throwIf state.selfIsUpdatingTokenPool 1n with 
Ok t0 -> (match throwIf (lebN param.remove_deadline (current_slot chain)) 1n with 
Ok t1 -> (match throwIf ((fun (x : tez) -> 0tez < x) (ctx_amount ctx)) 1n with 
Ok t2 -> (match divN_res (multN param.lqtBurned state.xtzPool) state.lqtTotal with 
Ok t3 -> (match divN_res (multN param.lqtBurned state.tokenPool) state.lqtTotal with 
Ok t4 -> (match throwIf (ltbN t3 param.minXtzWithdrawn) 1n with 
Ok t5 -> (match throwIf (ltbN t4 param.minTokensWithdrawn) 1n with 
Ok t6 -> (match sub state.lqtTotal param.lqtBurned with 
Ok t7 -> (match sub state.tokenPool t4 with 
Ok t8 -> (match sub state.xtzPool t3 with 
Ok t9 -> (match mint_or_burn state (ctx_from ctx) (subInt 0 (z_of_N param.lqtBurned)) with 
Ok t10 -> (let op_token = token_transfer state (ctx_contract_address ctx) param.liquidity_to t4 in 
match xtz_transfer param.liquidity_to t3 with 
Ok t11 -> (let new_state = ({tokenPool = t8; xtzPool = t9; lqtTotal = t7; selfIsUpdatingTokenPool = state.selfIsUpdatingTokenPool; freezeBaker = state.freezeBaker; manager = state.manager; tokenAddress = state.tokenAddress; tokenId = state.tokenId; lqtAddress = state.lqtAddress}: dexter2CPMM_State) in 
((Ok (new_state, (t10 :: (op_token :: (t11 :: ([]:operation list)))))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let xtz_to_token (chain : chain) (ctx : cctx) (state : dexter2CPMM_State) (param : dexter2CPMM_xtz_to_token_param) : dEX2Extract_Result = 
match throwIf state.selfIsUpdatingTokenPool 1n with 
Ok t0 -> (match throwIf (lebN param.xtt_deadline (current_slot chain)) 1n with 
Ok t1 -> (match divN_res (multN (multN (mutez_to_natural (ctx_amount ctx)) 997n) state.tokenPool) (addN (multN state.xtzPool 1000n) (multN (mutez_to_natural (ctx_amount ctx)) 997n)) with 
Ok t2 -> (match throwIf (ltbN t2 param.minTokensBought) 1n with 
Ok t3 -> (match sub state.tokenPool t2 with 
Ok t4 -> (let new_state = set_State_tokenPool (fun (a : nat) -> t4) (set_State_xtzPool (fun (a : nat) -> addN state.xtzPool (mutez_to_natural (ctx_amount ctx))) state) in 
let op = token_transfer state (ctx_contract_address ctx) param.tokens_to t2 in 
((Ok (new_state, (op :: ([]:operation list)))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let token_to_xtz (chain : chain) (ctx : cctx) (state : dexter2CPMM_State) (param : dexter2CPMM_token_to_xtz_param) : dEX2Extract_Result = 
match throwIf state.selfIsUpdatingTokenPool 1n with 
Ok t0 -> (match throwIf (lebN param.ttx_deadline (current_slot chain)) 1n with 
Ok t1 -> (match throwIf ((fun (x : tez) -> 0tez < x) (ctx_amount ctx)) 1n with 
Ok t2 -> (match divN_res (multN (multN param.tokensSold 997n) state.xtzPool) (addN (multN state.tokenPool 1000n) (multN param.tokensSold 997n)) with 
Ok t3 -> (match throwIf (ltbN t3 param.minXtzBought) 1n with 
Ok t4 -> (match sub state.xtzPool t3 with 
Ok t5 -> (let op_token = token_transfer state (ctx_from ctx) (ctx_contract_address ctx) param.tokensSold in 
match xtz_transfer param.xtz_to t3 with 
Ok t6 -> (let new_state = set_State_xtzPool (fun (a : nat) -> t5) (set_State_tokenPool (fun (a : nat) -> addN state.tokenPool param.tokensSold) state) in 
((Ok (new_state, (op_token :: (t6 :: ([]:operation list))))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let set_State_freezeBaker (f : bool -> bool) (r : dexter2CPMM_State) : dexter2CPMM_State = 
({tokenPool = (tokenPool r); xtzPool = (xtzPool r); lqtTotal = (lqtTotal r); selfIsUpdatingTokenPool = (selfIsUpdatingTokenPool r); freezeBaker = (f (freezeBaker r)); manager = (manager r); tokenAddress = (tokenAddress r); tokenId = (tokenId r); lqtAddress = (lqtAddress r)}: dexter2CPMM_State)

let set_baker (ctx : cctx) (state : dexter2CPMM_State) (param : dexter2CPMM_set_baker_param) : dEX2Extract_Result = 
match throwIf state.selfIsUpdatingTokenPool 1n with 
Ok t0 -> (match throwIf ((fun (x : tez) -> 0tez < x) (ctx_amount ctx)) 1n with 
Ok t1 -> (match throwIf (not (eq_addr (ctx_from ctx) state.manager)) 1n with 
Ok t2 -> (match throwIf state.freezeBaker 1n with 
Ok t3 -> ((Ok ((set_State_freezeBaker (fun (a : bool) -> param.freezeBaker_) state), ((fun (x : key_hash option) -> [Tezos.set_delegate x]) param.baker))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let set_State_manager (f : address -> address) (r : dexter2CPMM_State) : dexter2CPMM_State = 
({tokenPool = (tokenPool r); xtzPool = (xtzPool r); lqtTotal = (lqtTotal r); selfIsUpdatingTokenPool = (selfIsUpdatingTokenPool r); freezeBaker = (freezeBaker r); manager = (f (manager r)); tokenAddress = (tokenAddress r); tokenId = (tokenId r); lqtAddress = (lqtAddress r)}: dexter2CPMM_State)

let set_manager (ctx : cctx) (state : dexter2CPMM_State) (new_manager : address) : dEX2Extract_Result = 
match throwIf state.selfIsUpdatingTokenPool 1n with 
Ok t0 -> (match throwIf ((fun (x : tez) -> 0tez < x) (ctx_amount ctx)) 1n with 
Ok t1 -> (match throwIf (not (eq_addr (ctx_from ctx) state.manager)) 1n with 
Ok t2 -> ((Ok ((set_State_manager (fun (a : address) -> new_manager) state), ([]:operation list))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let set_State_lqtAddress (f : address -> address) (r : dexter2CPMM_State) : dexter2CPMM_State = 
({tokenPool = (tokenPool r); xtzPool = (xtzPool r); lqtTotal = (lqtTotal r); selfIsUpdatingTokenPool = (selfIsUpdatingTokenPool r); freezeBaker = (freezeBaker r); manager = (manager r); tokenAddress = (tokenAddress r); tokenId = (tokenId r); lqtAddress = (f (lqtAddress r))}: dexter2CPMM_State)

let set_lqt_address (ctx : cctx) (state : dexter2CPMM_State) (new_lqt_address : address) : dEX2Extract_Result = 
match throwIf state.selfIsUpdatingTokenPool 1n with 
Ok t0 -> (match throwIf ((fun (x : tez) -> 0tez < x) (ctx_amount ctx)) 1n with 
Ok t1 -> (match throwIf (not (eq_addr (ctx_from ctx) state.manager)) 1n with 
Ok t2 -> (match throwIf (not (eq_addr state.lqtAddress ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address))) 1n with 
Ok t3 -> ((Ok ((set_State_lqtAddress (fun (a : address) -> new_lqt_address) state), ([]:operation list))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let update_token_pool (ctx : cctx) (state : dexter2CPMM_State) : dEX2Extract_Result = 
match throwIf (not (eq_addr (ctx_from ctx) (ctx_origin ctx))) 1n with 
Ok t0 -> (match throwIf ((fun (x : tez) -> 0tez < x) (ctx_amount ctx)) 1n with 
Ok t1 -> (match throwIf state.selfIsUpdatingTokenPool 1n with 
Ok t2 -> (let balance_of_request = ({owner = (ctx_contract_address ctx); bal_req_token_id = state.tokenId}: fA2LegacyInterface_balance_of_request) in 
let balance_of_param = ({bal_requests = (balance_of_request :: ([]:fA2LegacyInterface_balance_of_request list)); bal_callback = ({blob = (None:fA2LegacyInterface_balance_of_response list option); return_addr = (ctx_contract_address ctx)}: fA2LegacyInterface_balance_of_response list fA2LegacyInterface_callback)}: fA2LegacyInterface_balance_of_param) in 
let op = call_to_token state.tokenAddress 0n (FA2T_msg_balance_of balance_of_param) in 
((Ok ((set_State_selfIsUpdatingTokenPool (fun (a : bool) -> true) state), (op :: ([]:operation list)))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let token_to_token (chain : chain) (ctx : cctx) (state : dexter2CPMM_State) (param : dexter2CPMM_token_to_token_param) : dEX2Extract_Result = 
match throwIf state.selfIsUpdatingTokenPool 1n with 
Ok t0 -> (match throwIf ((fun (x : tez) -> 0tez < x) (ctx_amount ctx)) 1n with 
Ok t1 -> (match throwIf (lebN param.ttt_deadline (current_slot chain)) 1n with 
Ok t2 -> (match divN_res (multN (multN param.tokensSold_ 997n) state.xtzPool) (addN (multN state.tokenPool 1000n) (multN param.tokensSold_ 997n)) with 
Ok t3 -> (match sub state.xtzPool t3 with 
Ok t4 -> (let new_state = set_State_xtzPool (fun (a : nat) -> t4) (set_State_tokenPool (fun (a : nat) -> addN state.tokenPool param.tokensSold_) state) in 
let op1 = token_transfer state (ctx_from ctx) (ctx_contract_address ctx) param.tokensSold_ in 
let op2 = call_to_token param.outputDexterContract t3 (FA2T_other_msg (Dext_XtzToToken ({tokens_to = param.to_; minTokensBought = param.minTokensBought_; xtt_deadline = param.ttt_deadline}: dexter2CPMM_xtz_to_token_param))) in 
((Ok (new_state, (op1 :: (op2 :: ([]:operation list))))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let default_ (ctx : cctx) (state : dexter2CPMM_State) : dEX2Extract_Result = 
match throwIf state.selfIsUpdatingTokenPool 1n with 
Ok t0 -> (let new_state = set_State_xtzPool (fun (a : nat) -> addN state.xtzPool (mutez_to_natural (ctx_amount ctx))) state in 
((Ok (new_state, ([]:operation list))):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result))
 | Err e0 -> ((Err e0):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)

let receive_cpmm (chain : chain) (ctx : cctx) (state : dexter2CPMM_State) (maybe_msg : dexter2CPMM_Msg option) : dEX2Extract_Result = 
match maybe_msg with 
Some m0 -> (match m0 with 
FA2T_receive_balance_of_param responses0 -> (update_token_pool_internal ctx state responses0)
 | FA2T_receive_total_supply_param l0 -> ((Err 1n):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)
 | FA2T_receive_metadata_callback l0 -> ((Err 1n):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)
 | FA2T_receive_is_operator i0 -> ((Err 1n):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)
 | FA2T_receive_permissions_descri p0 -> ((Err 1n):((dexter2CPMM_State * operation list), dexter2CPMM_Error) result)
 | FA2T_other_msg d0 -> (match d0 with 
Dext_AddLiquidity param0 -> (add_liquidity chain ctx state param0)
 | Dext_RemoveLiquidity param0 -> (remove_liquidity chain ctx state param0)
 | Dext_XtzToToken param0 -> (xtz_to_token chain ctx state param0)
 | Dext_TokenToXtz param0 -> (token_to_xtz chain ctx state param0)
 | Dext_SetBaker param0 -> (set_baker ctx state param0)
 | Dext_SetManager param0 -> (set_manager ctx state param0)
 | Dext_SetLqtAddress param0 -> (set_lqt_address ctx state param0)
 | Dext_UpdateTokenPool  -> (update_token_pool ctx state)
 | Dext_TokenToToken param0 -> (token_to_token chain ctx state param0)))
 | None  -> (default_ ctx state)

let receive_ (chain : chain) (ctx : cctx) (state : dexter2CPMM_State) (maybe_msg : dexter2CPMM_Msg option) : ((operation list * dexter2CPMM_State), dexter2CPMM_Error) result = 
match receive_cpmm chain ctx state maybe_msg with 
Ok x0 -> ((Ok (x0.1, x0.0)):((operation list * dexter2CPMM_State), dexter2CPMM_Error) result)
 | Err e0 -> ((Err e0):((operation list * dexter2CPMM_State), dexter2CPMM_Error) result)

let init (setup : dexter2CPMM_Setup) : (dexter2CPMM_State, dexter2CPMM_Error) result = let inner (setup : dexter2CPMM_Setup) :(dexter2CPMM_State, dexter2CPMM_Error) result = 
((Ok ({tokenPool = 0n; xtzPool = 0n; lqtTotal = setup.lqtTotal_; selfIsUpdatingTokenPool = false; freezeBaker = false; manager = setup.manager_; tokenAddress = setup.tokenAddress_; tokenId = setup.tokenId_; lqtAddress = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address)}: dexter2CPMM_State)):(dexter2CPMM_State, dexter2CPMM_Error) result) in
match (inner setup) with
  Ok v -> Ok v
| Err e -> (failwith e: (dexter2CPMM_State, dexter2CPMM_Error) result)


type return = (operation) list * dexter2CPMM_State

let main (p, st : dexter2CPMM_Msg option * dexter2CPMM_State) : return = 
   (match (receive_ dummy_chain cctx_instance  st p) with 
      Ok v -> (v.0, v.1)
    | Err e -> (failwith e : return))
