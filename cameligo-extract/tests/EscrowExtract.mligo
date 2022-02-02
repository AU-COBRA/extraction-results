
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

type setup = 
  Build_setup of address


type nextStep = 
  Buyer_commit
| Buyer_confirm
| Withdrawals
| No_next_step


type state = 
  Build_state of (nat * nextStep * address * address * tez * tez)


type msg = 
  Commit_money
| Confirm_item_received
| Withdraw


let setup_buyer(s : setup) : address = match s with 
Build_setup (setup_buyer) -> setup_buyer

let init(chain : chain) (ctx : cctx) (setup : setup) :  (state) option = let seller = ctx_from ctx in 
let buyer = setup_buyer setup in 
match if eq_addr buyer seller then (None: (unit) option) else Some (()) with 
Some (val0) -> (match if eqTez (ctx_amount ctx) 0tez then (None: (unit) option) else Some (()) with 
Some (val1) -> (match if evenTez (ctx_amount ctx) then Some (()) else (None: (unit) option) with 
Some (val2) -> (Some ((Build_state ((current_slot chain), Buyer_commit, seller, buyer, 0tez, 0tez))))
 | None  -> (None: (state) option))
 | None  -> (None: (state) option))
 | None  -> (None: (state) option)

let next_step(s : state) : nextStep = match s with 
Build_state (last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) -> next_step

let buyer(s : state) : address = match s with 
Build_state (last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) -> buyer

let last_action(s : state) : nat = match s with 
Build_state (last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) -> last_action

let seller(s : state) : address = match s with 
Build_state (last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) -> seller

let seller_withdrawable(s : state) : tez = match s with 
Build_state (last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) -> seller_withdrawable

let buyer_withdrawable(s : state) : tez = match s with 
Build_state (last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) -> buyer_withdrawable

let set_State_last_action(f : nat -> nat) (r : state) : state = Build_state ((f (last_action r)), (next_step r), (seller r), (buyer r), (seller_withdrawable r), (buyer_withdrawable r))

let set_State_next_step(f : nextStep -> nextStep) (r : state) : state = Build_state ((last_action r), (f (next_step r)), (seller r), (buyer r), (seller_withdrawable r), (buyer_withdrawable r))

let set_State_seller_withdrawable(f : tez -> tez) (r : state) : state = Build_state ((last_action r), (next_step r), (seller r), (buyer r), (f (seller_withdrawable r)), (buyer_withdrawable r))

let set_State_buyer_withdrawable(f : tez -> tez) (r : state) : state = Build_state ((last_action r), (next_step r), (seller r), (buyer r), (seller_withdrawable r), (f (buyer_withdrawable r)))

let receive(chain : chain) (ctx : cctx) (state : state) (msg :  (msg) option) :  ((state *  (operation) list)) option = match msg with 
Some (m) -> (match m with 
Commit_money  -> (match next_step state with 
Buyer_commit  -> (let item_price = divTez (subTez (ctx_contract_balance ctx) (ctx_amount ctx)) 2tez in 
let expected = multTez item_price 2tez in 
match if eq_addr (ctx_from ctx) (buyer state) then Some (()) else (None: (unit) option) with 
Some (val0) -> (match if eqTez (ctx_amount ctx) expected then Some (()) else (None: (unit) option) with 
Some (val1) -> (Some ( ((set_State_last_action (fun (a : nat) -> current_slot chain) (set_State_next_step (fun (a : nextStep) -> Buyer_confirm) state)), ([]: (operation) list))))
 | None  -> (None: ((state *  (operation) list)) option))
 | None  -> (None: ((state *  (operation) list)) option))
 | Buyer_confirm  -> (None: ((state *  (operation) list)) option)
 | Withdrawals  -> (None: ((state *  (operation) list)) option)
 | No_next_step  -> (None: ((state *  (operation) list)) option))
 | Confirm_item_received  -> (match next_step state with 
Buyer_commit  -> (None: ((state *  (operation) list)) option)
 | Buyer_confirm  -> (let item_price = divTez (ctx_contract_balance ctx) 4tez in 
match if eq_addr (ctx_from ctx) (buyer state) then Some (()) else (None: (unit) option) with 
Some (val0) -> (match if eqTez (ctx_amount ctx) 0tez then Some (()) else (None: (unit) option) with 
Some (val1) -> (let new_state = set_State_seller_withdrawable (fun (a : tez) -> multTez item_price 3tez) (set_State_buyer_withdrawable (fun (a : tez) -> item_price) (set_State_next_step (fun (a : nextStep) -> Withdrawals) state)) in 
Some ( (new_state, ([]: (operation) list))))
 | None  -> (None: ((state *  (operation) list)) option))
 | None  -> (None: ((state *  (operation) list)) option))
 | Withdrawals  -> (None: ((state *  (operation) list)) option)
 | No_next_step  -> (None: ((state *  (operation) list)) option))
 | Withdraw  -> (match next_step state with 
Buyer_commit  -> (match if eqTez (ctx_amount ctx) 0tez then Some (()) else (None: (unit) option) with 
Some (val0) -> (match if ltbN (addN (last_action state) 50n) (current_slot chain) then (None: (unit) option) else Some (()) with 
Some (val1) -> (match if eq_addr (ctx_from ctx) (seller state) then Some (()) else (None: (unit) option) with 
Some (val2) -> (let balance0 = ctx_contract_balance ctx in 
Some ( ((set_State_next_step (fun (a : nextStep) -> No_next_step) state), (Tezos.transaction unit balance0 (get_contract_unit (seller state)) :: ([]: (operation) list)))))
 | None  -> (None: ((state *  (operation) list)) option))
 | None  -> (None: ((state *  (operation) list)) option))
 | None  -> (None: ((state *  (operation) list)) option))
 | Buyer_confirm  -> (None: ((state *  (operation) list)) option)
 | Withdrawals  -> (match if eqTez (ctx_amount ctx) 0tez then Some (()) else (None: (unit) option) with 
Some (val0) -> (let from = ctx_from ctx in 
match if eq_addr from (buyer state) then Some ( ((buyer_withdrawable state), (set_State_buyer_withdrawable (fun (a : tez) -> 0tez) state))) else if eq_addr from (seller state) then Some ( ((seller_withdrawable state), (set_State_seller_withdrawable (fun (a : tez) -> 0tez) state))) else (None: ((tez * state)) option) with 
Some (val1) -> (match val1 with 
 (to_pay, new_state) -> (match if gtbTez to_pay 0tez then Some (()) else (None: (unit) option) with 
Some (val2) -> (let new_state0 = if andb (eqTez (buyer_withdrawable new_state) 0tez) (eqTez (seller_withdrawable new_state) 0tez) then set_State_next_step (fun (a : nextStep) -> No_next_step) new_state else new_state in 
Some ( (new_state0, (Tezos.transaction unit to_pay (get_contract_unit (ctx_from ctx)) :: ([]: (operation) list)))))
 | None  -> (None: ((state *  (operation) list)) option)))
 | None  -> (None: ((state *  (operation) list)) option))
 | None  -> (None: ((state *  (operation) list)) option))
 | No_next_step  -> (None: ((state *  (operation) list)) option)))
 | None  -> (None: ((state *  (operation) list)) option)

let escrow_receive(c : chain) (cctx : cctx) (s : state) (msg :  (msg) option) :  (( (operation) list * state)) option = match receive c cctx s msg with 
Some (p) -> (match p with 
 (s0, acts) -> (Some ( (acts, s0))))
 | None  -> (None: (( (operation) list * state)) option)

let init (s : (setup * chain)) : state = 

let inner (cctx : cctx) (s : (setup * chain)) : (state) option = 
init s.1 cctx s.0 in
let ctx = cctx_instance in
match (inner ctx s) with
  Some v -> v
| None -> (failwith (""): state)
type init_args_ty = (setup * chain)
let init_wrapper (args : init_args_ty) =
  init args


type return = (operation) list * (state option)
type parameter_wrapper =
  Init of init_args_ty
| Call of msg option

let wrapper (param, st : parameter_wrapper * (state) option) : return =
  match param with  
    Init init_args -> (([]: operation list), Some (init init_args))
  | Call p -> (
    match st with
      Some st -> (match (escrow_receive dummy_chain cctx_instance  st p) with   
                    Some v -> (v.0, Some v.1)
                  | None -> (failwith ("") : return))
    | None -> (failwith ("cannot call this endpoint before Init has been called"): return))
let main (action, st : parameter_wrapper * state option) : return = wrapper (action, st)
