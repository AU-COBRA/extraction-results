
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

let test_account : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
let init_storage : (timestamp * (tez * address)) =
          (Tezos.get_now (), (42tez,("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address)))

type time_coq = 
  Time_coq of nat


type msg_coq = 
  Donate_coq
| GetFunds_coq
| Claim_coq


let leb_time (t1 : time_coq) (t2 : time_coq) : bool = 
match t1 with 
Time_coq n10 -> (match t2 with 
Time_coq n20 -> (lebN n10 n20))

let update_contribs (f_st : ((time_coq * (tez * address)) * ((address,tez) map * bool))) (cs : (address,tez) map) : ((time_coq * (tez * address)) * ((address,tez) map * bool)) = 
(f_st.0, (cs, f_st.1.1))

let ltb_time (t1 : time_coq) (t2 : time_coq) : bool = 
match t1 with 
Time_coq n10 -> (match t2 with 
Time_coq n20 -> (ltbN n10 n20))

let maybe_bind_unit (o : unit option) (b : (operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))) option) : (operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))) option = 
match o with 
Some a -> b
 | None  -> (None:(operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))) option)

let validate (tx_amount : tez) : unit option = 
if eqTez 0tez tx_amount then Some () else (None:unit option)

let set_done (f_st : ((time_coq * (tez * address)) * ((address,tez) map * bool))) : ((time_coq * (tez * address)) * ((address,tez) map * bool)) = 
(f_st.0, (f_st.1.0, true))

let receive (m : msg_coq) (s : ((time_coq * (tez * address)) * ((address,tez) map * bool))) (ctx : (time_coq * (address * (tez * tez)))) : (operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))) option = 
match m with 
Donate_coq  -> (if leb_time ctx.0 s.0.0 then match Map.find_opt ctx.1.0 s.1.0 with 
Some v0 -> (Some (([]:operation list), (update_contribs s (Map.add ctx.1.0 (addTez v0 ctx.1.1.0) s.1.0))))
 | None  -> (Some (([]:operation list), (update_contribs s (Map.add ctx.1.0 ctx.1.1.0 s.1.0)))) else (None:(operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))) option))
 | GetFunds_coq  -> (if andb (andb (eq_addr s.0.1.1 ctx.1.0) (ltb_time s.0.0 ctx.0)) (leTez s.0.1.0 ctx.1.1.1) then maybe_bind_unit (validate ctx.1.1.0) (Some ((Tezos.transaction unit ctx.1.1.1 (get_contract_unit ctx.1.0) :: ([]:operation list)), (set_done s))) else (None:(operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))) option))
 | Claim_coq  -> (if andb (andb (ltb_time s.0.0 ctx.0) (ltTez ctx.1.1.1 s.0.1.0)) (not s.1.1) then match Map.find_opt ctx.1.0 s.1.0 with 
Some v0 -> (maybe_bind_unit (validate ctx.1.1.0) (Some ((Tezos.transaction unit v0 (get_contract_unit ctx.1.0) :: ([]:operation list)), (update_contribs s (Map.add ctx.1.0 0tez s.1.0)))))
 | None  -> (None:(operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))) option) else (None:(operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))) option))

let result_of_option(type t e) (o : t option) (err : e) : (t, e) result = 
match o with 
Some a0 -> ((Ok a0):(t, e) result)
 | None  -> ((Err err):(t, e) result)

let crowdfunding_receive_inner (c : chain) (ctx : cctx) (params : msg_coq) (st : ((time_coq * (tez * address)) * ((address,tez) map * bool))) : ((operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))), nat) result = 
let res = receive params st ((Time_coq (current_slot c)), (((fun (x : address) -> x) (ctx_from ctx)), ((ctx_amount ctx), (ctx_contract_balance ctx)))) in 
result_of_option res 0n

let crowdfunding_receive (c : chain) (ctx : cctx) (st : ((time_coq * (tez * address)) * ((address,tez) map * bool))) (msg : msg_coq option) : ((operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))), nat) result = 
match msg with 
Some msg0 -> (crowdfunding_receive_inner c ctx msg0 st)
 | None  -> ((Err 0n):((operation list * ((time_coq * (tez * address)) * ((address,tez) map * bool))), nat) result)

let init (setup : (time_coq * (tez * address))) : (((time_coq * (tez * address)) * ((address,tez) map * bool)), nat) result = let inner (setup : (time_coq * (tez * address))) :(((time_coq * (tez * address)) * ((address,tez) map * bool)), nat) result = 
((Ok (setup, ((Map.empty:(address,tez) map), false))):(((time_coq * (tez * address)) * ((address,tez) map * bool)), nat) result) in
match (inner setup) with
  Ok v -> Ok v
| Err e -> (failwith e: (((time_coq * (tez * address)) * ((address,tez) map * bool)), nat) result)

type storage = ((time_coq * (tez * address)) * ((address,tez) map * bool))

type return = (operation) list * storage

let main (p, st : msg_coq option * storage) : return = 
   (match (crowdfunding_receive dummy_chain cctx_instance  st p) with 
      Ok v -> (v.0, v.1)
    | Err e -> (failwith e : return))
