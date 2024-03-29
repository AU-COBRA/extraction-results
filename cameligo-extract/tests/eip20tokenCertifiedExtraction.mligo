
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

type tokenValue = int

type setup = 
  Build_setup of (address * tokenValue)


type state = 
  Build_state of (tokenValue * (address, tokenValue) map * (address, (address, tokenValue) map) map)


type error = nat

type msg = 
  Transfer of (address * tokenValue)
| Transfer_from of (address * address * tokenValue)
| Approve of (address * tokenValue)


let init_amount (s : setup) : tokenValue = 
match s with 
Build_setup (init_amount0, owner0) -> owner0

let owner (s : setup) : address = 
match s with 
Build_setup (init_amount0, owner0) -> init_amount0

let default_error  : error = 
1n

let error(type t) (t : unit) : (t, error) result = 
((Err default_error):(t, error) result)

let without_actions(type t e) (x : (t, e) result) : ((t * operation list), e) result = 
match x with 
Ok t0 -> ((Ok (t0, ([]:operation list))):((t * operation list), e) result)
 | Err e0 -> ((Err e0):((t * operation list), e) result)

let balances (s : state) : (address, tokenValue) map = 
match s with 
Build_state (allowances0, balances0, total_supply0) -> balances0

let increment_balance (m : (address, tokenValue) map) (addr : address) (inc : tokenValue) : (address, tokenValue) map = 
match Map.find_opt addr m with 
Some old0 -> (Map.add addr (addInt old0 inc) m)
 | None  -> (Map.add addr inc m)

let total_supply (s : state) : tokenValue = 
match s with 
Build_state (allowances0, balances0, total_supply0) -> allowances0

let allowances (s : state) : (address, (address, tokenValue) map) map = 
match s with 
Build_state (allowances0, balances0, total_supply0) -> total_supply0

let set_State_balances (f : (address, tokenValue) map -> (address, tokenValue) map) (r : state) : state = 
Build_state ((total_supply r), (f (balances r)), (allowances r))

let try_transfer (from : address) (to0 : address) (amount0 : tokenValue) (state : state) : (state, error) result = 
let from_balance = match Map.find_opt from (balances state) with 
Some v0 -> v0
 | None  -> 0 in 
if ltInt from_balance amount0 then error () else let new_balances = Map.add from (subIntTruncated from_balance amount0) (balances state) in 
let new_balances0 = increment_balance new_balances to0 amount0 in 
((Ok (set_State_balances (fun (a : (address, tokenValue) map) -> new_balances0) state)):(state, error) result)

let result_of_option(type t e) (o : t option) (err : e) : (t, e) result = 
match o with 
Some a0 -> ((Ok a0):(t, e) result)
 | None  -> ((Err err):(t, e) result)

let set_State_allowances (f : (address, (address, tokenValue) map) map -> (address, (address, tokenValue) map) map) (r : state) : state = 
Build_state ((total_supply r), (balances r), (f (allowances r)))

let try_transfer_from (delegate : address) (from : address) (to0 : address) (amount0 : tokenValue) (state : state) : (state, error) result = 
match result_of_option (Map.find_opt from (allowances state)) default_error with 
Ok t0 -> (match result_of_option (Map.find_opt delegate t0) default_error with 
Ok t1 -> (let from_balance = match Map.find_opt from (balances state) with 
Some v0 -> v0
 | None  -> 0 in 
if orb (ltInt t1 amount0) (ltInt from_balance amount0) then error () else let new_allowances = Map.add delegate (subIntTruncated t1 amount0) t0 in 
let new_balances = Map.add from (subIntTruncated from_balance amount0) (balances state) in 
let new_balances0 = increment_balance new_balances to0 amount0 in 
((Ok (set_State_allowances (Map.add from new_allowances) (set_State_balances (fun (a : (address, tokenValue) map) -> new_balances0) state))):(state, error) result))
 | Err e0 -> ((Err e0):(state, error) result))
 | Err e0 -> ((Err e0):(state, error) result)

let try_approve (caller : address) (delegate : address) (amount0 : tokenValue) (state : state) : (state, error) result = 
match Map.find_opt caller (allowances state) with 
Some caller_allowances0 -> ((Ok (set_State_allowances (Map.add caller (Map.add delegate amount0 caller_allowances0)) state)):(state, error) result)
 | None  -> ((Ok (set_State_allowances (Map.add caller (Map.add delegate amount0 (Map.empty: (address, tokenValue) map))) state)):(state, error) result)

let receive (ctx : cctx) (state : state) (maybe_msg : msg option) : ((state * operation list), error) result = 
let sender0 = ctx_from ctx in 
if gtbTez (ctx_amount ctx) 0tez then error () else match maybe_msg with 
Some m0 -> (match m0 with 
Transfer (amount0, to0) -> (without_actions (try_transfer sender0 amount0 to0 state))
 | Transfer_from (amount0, to0, from0) -> (without_actions (try_transfer_from sender0 amount0 to0 from0 state))
 | Approve (amount0, delegate0) -> (without_actions (try_approve sender0 amount0 delegate0 state)))
 | None  -> (error ())

let receive_ (chain : chain) (ctx : cctx) (state : state) (maybe_msg : msg option) : ((operation list * state), error) result = 
match receive ctx state maybe_msg with 
Ok x0 -> ((Ok (x0.1, x0.0)):((operation list * state), error) result)
 | Err e0 -> ((Err e0):((operation list * state), error) result)

let init (setup : setup) : (state, error) result = let inner (setup : setup) :(state, error) result = 
((Ok (Build_state ((init_amount setup), (Map.add (owner setup) (init_amount setup) (Map.empty: (address, tokenValue) map)), (Map.empty: (address, (address, tokenValue) map) map)))):(state, error) result) in
match (inner setup) with
  Ok v -> Ok v
| Err e -> (failwith e: (state, error) result)


type return = (operation) list * state

let main (p, st : msg option * state) : return = 
   (match (receive_ dummy_chain cctx_instance  st p) with 
      Ok v -> (v.0, v.1)
    | Err e -> (failwith e : return))
