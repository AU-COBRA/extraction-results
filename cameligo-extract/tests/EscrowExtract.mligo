
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

type setup = 
  Build_setup of address


type nextStep = 
  Buyer_commit
| Buyer_confirm
| Withdrawals
| No_next_step


type state = 
  Build_state of (nat * nextStep * address * address * tez * tez)


type error = nat

type msg = 
  Commit_money
| Confirm_item_received
| Withdraw


let setup_buyer (s : setup) : address = 
match s with 
Build_setup setup_buyer0 -> setup_buyer0

let next_step (s : state) : nextStep = 
match s with 
Build_state (buyer_withdrawable0, seller_withdrawable0, buyer0, seller0, next_step0, last_action0) -> seller_withdrawable0

let result_of_option(type t e) (o : t option) (err : e) : (t, e) result = 
match o with 
Some a0 -> ((Ok a0):(t, e) result)
 | None  -> ((Err err):(t, e) result)

let buyer (s : state) : address = 
match s with 
Build_state (buyer_withdrawable0, seller_withdrawable0, buyer0, seller0, next_step0, last_action0) -> seller0

let last_action (s : state) : nat = 
match s with 
Build_state (buyer_withdrawable0, seller_withdrawable0, buyer0, seller0, next_step0, last_action0) -> buyer_withdrawable0

let seller (s : state) : address = 
match s with 
Build_state (buyer_withdrawable0, seller_withdrawable0, buyer0, seller0, next_step0, last_action0) -> buyer0

let seller_withdrawable (s : state) : tez = 
match s with 
Build_state (buyer_withdrawable0, seller_withdrawable0, buyer0, seller0, next_step0, last_action0) -> next_step0

let buyer_withdrawable (s : state) : tez = 
match s with 
Build_state (buyer_withdrawable0, seller_withdrawable0, buyer0, seller0, next_step0, last_action0) -> last_action0

let set_State_last_action (f : nat -> nat) (r : state) : state = 
Build_state ((f (last_action r)), (next_step r), (seller r), (buyer r), (seller_withdrawable r), (buyer_withdrawable r))

let set_State_next_step (f : nextStep -> nextStep) (r : state) : state = 
Build_state ((last_action r), (f (next_step r)), (seller r), (buyer r), (seller_withdrawable r), (buyer_withdrawable r))

let set_State_seller_withdrawable (f : tez -> tez) (r : state) : state = 
Build_state ((last_action r), (next_step r), (seller r), (buyer r), (f (seller_withdrawable r)), (buyer_withdrawable r))

let set_State_buyer_withdrawable (f : tez -> tez) (r : state) : state = 
Build_state ((last_action r), (next_step r), (seller r), (buyer r), (seller_withdrawable r), (f (buyer_withdrawable r)))

let receive (chain : chain) (ctx : cctx) (state : state) (msg : msg option) : ((state * operation list), error) result = 
match msg with 
Some m0 -> (match m0 with 
Commit_money  -> (match next_step state with 
Buyer_commit  -> (match result_of_option (subTez (ctx_contract_balance ctx) (ctx_amount ctx)) 1n with 
Ok t0 -> (let item_price = divTez t0 2tez in 
let expected = multTez item_price 2tez in 
match if eq_addr (ctx_from ctx) (buyer state) then ((Ok ()):(unit, error) result) else ((Err 1n):(unit, error) result) with 
Ok t1 -> (match if eqTez (ctx_amount ctx) expected then ((Ok ()):(unit, error) result) else ((Err 1n):(unit, error) result) with 
Ok t2 -> ((Ok ((set_State_last_action (fun (a : nat) -> current_slot chain) (set_State_next_step (fun (a : nextStep) -> Buyer_confirm) state)), ([]:operation list))):((state * operation list), error) result)
 | Err e0 -> ((Err e0):((state * operation list), error) result))
 | Err e0 -> ((Err e0):((state * operation list), error) result))
 | Err e0 -> ((Err e0):((state * operation list), error) result))
 | Buyer_confirm  -> ((Err 1n):((state * operation list), error) result)
 | Withdrawals  -> ((Err 1n):((state * operation list), error) result)
 | No_next_step  -> ((Err 1n):((state * operation list), error) result))
 | Confirm_item_received  -> (match next_step state with 
Buyer_commit  -> ((Err 1n):((state * operation list), error) result)
 | Buyer_confirm  -> (let item_price = divTez (ctx_contract_balance ctx) 4tez in 
match if eq_addr (ctx_from ctx) (buyer state) then ((Ok ()):(unit, error) result) else ((Err 1n):(unit, error) result) with 
Ok t0 -> (match if eqTez (ctx_amount ctx) 0tez then ((Ok ()):(unit, error) result) else ((Err 1n):(unit, error) result) with 
Ok t1 -> (let new_state = set_State_seller_withdrawable (fun (a : tez) -> multTez item_price 3tez) (set_State_buyer_withdrawable (fun (a : tez) -> item_price) (set_State_next_step (fun (a : nextStep) -> Withdrawals) state)) in 
((Ok (new_state, ([]:operation list))):((state * operation list), error) result))
 | Err e0 -> ((Err e0):((state * operation list), error) result))
 | Err e0 -> ((Err e0):((state * operation list), error) result))
 | Withdrawals  -> ((Err 1n):((state * operation list), error) result)
 | No_next_step  -> ((Err 1n):((state * operation list), error) result))
 | Withdraw  -> (match next_step state with 
Buyer_commit  -> (match if eqTez (ctx_amount ctx) 0tez then ((Ok ()):(unit, error) result) else ((Err 1n):(unit, error) result) with 
Ok t0 -> (match if ltbN (addN (last_action state) 50n) (current_slot chain) then ((Err 1n):(unit, error) result) else ((Ok ()):(unit, error) result) with 
Ok t1 -> (match if eq_addr (ctx_from ctx) (seller state) then ((Ok ()):(unit, error) result) else ((Err 1n):(unit, error) result) with 
Ok t2 -> (let balance0 = ctx_contract_balance ctx in 
((Ok ((set_State_next_step (fun (a : nextStep) -> No_next_step) state), (Tezos.transaction unit balance0 (get_contract_unit (seller state)) :: ([]:operation list)))):((state * operation list), error) result))
 | Err e0 -> ((Err e0):((state * operation list), error) result))
 | Err e0 -> ((Err e0):((state * operation list), error) result))
 | Err e0 -> ((Err e0):((state * operation list), error) result))
 | Buyer_confirm  -> ((Err 1n):((state * operation list), error) result)
 | Withdrawals  -> (match if eqTez (ctx_amount ctx) 0tez then ((Ok ()):(unit, error) result) else ((Err 1n):(unit, error) result) with 
Ok t0 -> (let from = ctx_from ctx in 
match if eq_addr from (buyer state) then ((Ok ((buyer_withdrawable state), (set_State_buyer_withdrawable (fun (a : tez) -> 0tez) state))):((tez * state), error) result) else if eq_addr from (seller state) then ((Ok ((seller_withdrawable state), (set_State_seller_withdrawable (fun (a : tez) -> 0tez) state))):((tez * state), error) result) else ((Err 1n):((tez * state), error) result) with 
Ok t1 -> (match t1 with 
 (new_state0, to_pay0) -> (match if gtbTez new_state0 0tez then ((Ok ()):(unit, error) result) else ((Err 1n):(unit, error) result) with 
Ok t2 -> (let new_state1 = if andb (eqTez (buyer_withdrawable to_pay0) 0tez) (eqTez (seller_withdrawable to_pay0) 0tez) then set_State_next_step (fun (a : nextStep) -> No_next_step) to_pay0 else to_pay0 in 
((Ok (new_state1, (Tezos.transaction unit new_state0 (get_contract_unit (ctx_from ctx)) :: ([]:operation list)))):((state * operation list), error) result))
 | Err e0 -> ((Err e0):((state * operation list), error) result)))
 | Err e0 -> ((Err e0):((state * operation list), error) result))
 | Err e0 -> ((Err e0):((state * operation list), error) result))
 | No_next_step  -> ((Err 1n):((state * operation list), error) result)))
 | None  -> ((Err 1n):((state * operation list), error) result)

let escrow_receive (c : chain) (cctx : cctx) (s : state) (msg : msg option) : ((operation list * state), error) result = 
match receive c cctx s msg with 
Ok t0 -> (match t0 with 
 (acts0, s0) -> ((Ok (s0, acts0)):((operation list * state), error) result))
 | Err e0 -> ((Err e0):((operation list * state), error) result)

let init (s : ((address * setup) * nat)) : (state, error) result = let inner (s : ((address * setup) * nat)) :(state, error) result = 
let seller = s.0.0 in 
let setup = s.0.1 in 
let curr_slot = s.1 in 
let buyer = setup_buyer setup in 
if eq_addr buyer seller then ((Err 1n):(state, error) result) else ((Ok (Build_state (curr_slot, Buyer_commit, seller, buyer, 0tez, 0tez))):(state, error) result) in
match (inner s) with
  Ok v -> Ok v
| Err e -> (failwith e: (state, error) result)


type return = (operation) list * state

let main (p, st : msg option * state) : return = 
   (match (escrow_receive dummy_chain cctx_instance  st p) with   
      Ok v -> (v.0, v.1)
    | Err e -> (failwith e : return))
