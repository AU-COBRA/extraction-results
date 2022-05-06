
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

type tokenValue = int

type setup = 
  Build_setup of (address * tokenValue)


type state = 
  Build_state of (tokenValue * (address, tokenValue) map * (address, (address, tokenValue) map) map)


type msg = 
  Transfer of (address * tokenValue)
| Transfer_from of (address * address * tokenValue)
| Approve of (address * tokenValue)


let init_amount (s : setup) : tokenValue = 
match s with 
Build_setup (owner, init_amount) -> init_amount

let owner (s : setup) : address = 
match s with 
Build_setup (owner, init_amount) -> owner

let balances (s : state) : (address, tokenValue) map = 
match s with 
Build_state (total_supply, balances, allowances) -> balances

let increment_balance (m : (address, tokenValue) map) (addr : address) (inc : tokenValue) : (address, tokenValue) map = 
match Map.find_opt addr m with 
Some old -> (Map.add addr (addInt old inc) m)
 | None  -> (Map.add addr inc m)

let total_supply (s : state) : tokenValue = 
match s with 
Build_state (total_supply, balances, allowances) -> total_supply

let allowances (s : state) : (address, (address, tokenValue) map) map = 
match s with 
Build_state (total_supply, balances, allowances) -> allowances

let try_transfer (from : address) (to0 : address) (amount0 : tokenValue) (state : state) : state option = 
let from_balance = match Map.find_opt from (balances state) with 
Some v -> v
 | None  -> 0 in 
if ltInt from_balance amount0 then (None:state option) else let new_balances = Map.add from (subIntTruncated from_balance amount0) (balances state) in 
let new_balances0 = increment_balance new_balances to0 amount0 in 
Some (Build_state ((total_supply state), new_balances0, (allowances state)))

let try_transfer_from (delegate : address) (from : address) (to0 : address) (amount0 : tokenValue) (state : state) : state option = 
match Map.find_opt from (allowances state) with 
Some val0 -> (match Map.find_opt delegate val0 with 
Some val1 -> (let from_balance = match Map.find_opt from (balances state) with 
Some v -> v
 | None  -> 0 in 
if orb (ltInt val1 amount0) (ltInt from_balance amount0) then (None:state option) else let new_allowances = Map.add delegate (subIntTruncated val1 amount0) val0 in 
let new_balances = Map.add from (subIntTruncated from_balance amount0) (balances state) in 
let new_balances0 = increment_balance new_balances to0 amount0 in 
Some (Build_state ((total_supply (Build_state ((total_supply state), new_balances0, (allowances state)))), (balances (Build_state ((total_supply state), new_balances0, (allowances state)))), (Map.add from new_allowances (allowances (Build_state ((total_supply state), new_balances0, (allowances state))))))))
 | None  -> (None:state option))
 | None  -> (None:state option)

let try_approve (caller : address) (delegate : address) (amount0 : tokenValue) (state : state) : state option = 
match Map.find_opt caller (allowances state) with 
Some caller_allowances -> (Some (Build_state ((total_supply state), (balances state), (Map.add caller (Map.add delegate amount0 caller_allowances) (allowances state)))))
 | None  -> (Some (Build_state ((total_supply state), (balances state), (Map.add caller (Map.add delegate amount0 (Map.empty: (address, tokenValue) map)) (allowances state)))))

let receive (ctx : cctx) (state : state) (maybe_msg : msg option) : (state * operation list) option = 
let sender0 = ctx_from ctx in 
let without_actions = fun (o : state option) -> match o with 
Some a -> (Some (a, ([]:operation list)))
 | None  -> (None:(state * operation list) option) in 
if gtbTez (ctx_amount ctx) 0tez then (None:(state * operation list) option) else match maybe_msg with 
Some m -> (match m with 
Transfer (to0, amount0) -> (without_actions (try_transfer sender0 to0 amount0 state))
 | Transfer_from (from, to0, amount0) -> (without_actions (try_transfer_from sender0 from to0 amount0 state))
 | Approve (delegate, amount0) -> (without_actions (try_approve sender0 delegate amount0 state)))
 | None  -> (None:(state * operation list) option)

let receive_ (chain : chain) (ctx : cctx) (state : state) (maybe_msg : msg option) : (operation list * state) option = 
match receive ctx state maybe_msg with 
Some x -> (Some (x.1, x.0))
 | None  -> (None:(operation list * state) option)

let init (setup : setup) : state = let inner (setup : setup) :state option = 
Some (Build_state ((init_amount setup), (Map.add (owner setup) (init_amount setup) (Map.empty: (address, tokenValue) map)), (Map.empty: (address, (address, tokenValue) map) map))) in
match (inner setup) with
  Some v -> v
| None -> (failwith ("Init failed"): state)


type return = (operation) list * state

let main (p, st : msg option * state) : return = 
   (match (receive_ dummy_chain cctx_instance  st p) with   
      Some v -> (v.0, v.1)
    | None -> (failwith ("Contract returned None") : return))
