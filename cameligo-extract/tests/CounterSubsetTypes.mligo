
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
type 'a sig_ = 'a
let exist_ (a : _a) : _a = a
let id_func (a : _a) : _a = a
type chain = {
        chain_height     : nat;
        current_slot     : nat;
        finalized_height : nat;
        account_balance  : address -> nat
      }
let dummy_chain : chain = {
        chain_height     = 0n;
        current_slot     = 0n;
        finalized_height = 0n;
        account_balance  = fun (a : address) -> 0n
      }

type coq_sumbool = 
  Coq_left
| Coq_right


type storage = int

type coq_msg = 
  Coq_Inc of (int)
| Coq_Dec of (int)


let coq_bool_dec (b1 : bool) (b2 : bool) = (if b1 then fun (x : bool) -> if x then Coq_left else Coq_right else fun (x : bool) -> if x then Coq_right else Coq_left) b2

let coq_Transaction_none  = ([]: (operation) list)

let coq_inc_counter (st : storage) (inc :  (int) sig_) = exist_ ((addInt st (id_func inc)))

let coq_dec_counter (st : storage) (dec :  (int) sig_) = exist_ ((subInt st (id_func dec)))

let coq_counter (msg : coq_msg) (st : storage) = match msg with 
Coq_Inc (i) -> (match coq_bool_dec true (ltInt 0 i) with 
Coq_left  -> (Some ( (coq_Transaction_none, (id_func (coq_inc_counter st (exist_ (i)))))))
 | Coq_right  -> (None: ((operation list * storage)) option))
 | Coq_Dec (i) -> (match coq_bool_dec true (ltInt 0 i) with 
Coq_left  -> (Some ( (coq_Transaction_none, (id_func (coq_dec_counter st (exist_ (i)))))))
 | Coq_right  -> (None: ((operation list * storage)) option))

let coq_counter_wrapper (c : chain) (ctx : cctx) (s : storage) (m :  (coq_msg) option) = let c_ = c in 
let ctx_ = ctx in 
match m with 
Some (m0) -> (coq_counter m0 s)
 | None  -> (None: ((operation list * storage)) option)

let init (setup : int) : storage = 

let inner (ctx : cctx) (setup : int) : (storage) option = 
let ctx_ = ctx in 
Some (setup) in
let ctx = cctx_instance in
match (inner ctx setup) with
  Some v -> v
| None -> (failwith (""): storage)
type init_args_ty = int
let init_wrapper (args : init_args_ty) =
  init args


type return = (operation) list * (storage option)
type parameter_wrapper =
  Init of init_args_ty
| Call of coq_msg option

let wrapper (param, st : parameter_wrapper * (storage) option) : return =
  match param with  
    Init init_args -> (([]: operation list), Some (init init_args))
  | Call p -> (
    match st with
      Some st -> (match (coq_counter_wrapper dummy_chain cctx_instance  st p) with   
                    Some v -> (v.0, Some v.1)
                  | None -> (failwith ("") : return))
    | None -> (failwith ("cannot call this endpoint before Init has been called"): return))
let main (action, st : parameter_wrapper * storage option) : return = wrapper (action, st)
