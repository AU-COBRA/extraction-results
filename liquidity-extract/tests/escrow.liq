(*START*)
let[@inline] fst (p : 'a * 'b) : 'a = p.(0)
let[@inline] snd (p : 'a * 'b) : 'b = p.(1)

let[@inline] addInt (i : int) (j : int) = i + j
let[@inline] mulInt (i : int) (j : int) = i * j
let[@inline] subInt (i : int) (j : int) = i - j
let[@inline] leInt (i : int) (j : int) = i <= j
let[@inline] ltInt (i : int) (j : int) = i < j
let[@inline] eqInt (i : int) (j : int) = i = j
let[@inline] modInt(a : int)(b : int) : int = match a/b with | Some (_, r) -> int r | None -> 0
let rec powIntAcc((a,b,acc) : int*int*int) =
        if b <= 0 then acc
        else powIntAcc(a, (b-1), acc * a)
let powInt(a : int)(b : int) = powIntAcc(a,b,1)

let[@inline] addTez (n : tez) (m : tez) = n + m
let[@inline] subTez (n : tez) (m : tez) = n - m
let[@inline] leTez (a : tez ) (b : tez ) = a <= b
let[@inline] ltTez (a : tez ) (b : tez ) = a < b
let[@inline] gtTez (a : tez ) (b : tez ) = a > b
let[@inline] eqTez (a : tez ) (b : tez ) = a = b
let[@inline] evenTez (i : tez) = match i/2tz with | Some (_, r) -> r=0tz | None -> false
let tez_to_nat (a : tez) : nat =
let (n, _) = match a / 1DUN with
| Some qr -> qr
| None -> failwith () (* impossible case *)
in n
let[@inline] divTez (a : tez) (b : tez) : tez = match a/(tez_to_nat b) with Some(d,_) -> d | None -> 0tz
let[@inline] multTez (n : tez) (m : tez) : tez = n * tez_to_nat m

let[@inline] addNat (i : nat) (j : nat) = i + j
let[@inline] mulNat (i : nat) (j : nat) = i * j
let[@inline] subNat (i : nat) (j : nat) = i - j
let[@inline] leNat (i : nat) (j : nat) = i <= j
let[@inline] ltNat (i : nat) (j : nat) = i < j
let[@inline] lxorNat (i : nat) (j : nat) = i lxor j
let[@inline] eqNat (i : nat) (j : nat) = i = j

let[@inline] andb (a : bool ) (b : bool ) = a & b
let[@inline] orb (a : bool ) (b : bool ) = a || b

let[@inline] eqb_time (a1 : timestamp) (a2 : timestamp) = a1 = a2
let[@inline] leb_time (a1 : timestamp) (a2 : timestamp) = a1 <= a2
let[@inline] ltb_time (a1 : timestamp) (a2 : timestamp) = a1 < a2

let[@inline] eq_addr (a1 : address) (a2 : address) = a1 = a2

type 'a addrMap = (address, 'a) map

type ('t,'e) result =
  Ok of 't
| Err of 'e
type cctx = (timestamp * (address * (tez * tez)))
type chain = (nat * (nat * nat))

type msg = 
Commit_money
| Confirm_item_received
| Withdraw


type setup = address

type nextStep = 
Buyer_commit
| Buyer_confirm
| Withdrawals
| No_next_step


type state = {
last_action : nat;
next_step : nextStep;
seller : address;
buyer : address;
seller_withdrawable : tez;
buyer_withdrawable : tez
}

type  error = nat

let default_error  = 1p

let result_of_option (o : ( ('t) option)) (err : 'e) = match o with 
Some (a0) -> (Ok (a0))
 | None  -> (Err (err))

let subAmountOption (n : tez) (m : tez) = if ltTez n m then None else Some ((subTez n m))

let receive (chain : chain) (ctx : cctx) (state : state) (msg : ( (msg) option)) = match msg with 
Some (m0) -> (match m0 with 
Commit_money  -> (match state.next_step with 
Buyer_commit  -> (match result_of_option (subAmountOption ((fun (c : cctx) -> c.(1).(1).(0)) ctx) ((fun (c : cctx) -> c.(1).(1).(1)) ctx)) default_error with 
Ok (t0) -> (let item_price = divTez t0 2tz in 
let expected = multTez item_price 2tz in 
match if eq_addr ((fun (c : cctx) -> c.(1).(0)) ctx) state.buyer then Ok (()) else Err (default_error) with 
Ok (t1) -> (match if eqTez ((fun (c : cctx) -> c.(1).(1).(1)) ctx) expected then Ok (()) else Err (default_error) with 
Ok (t2) -> (Ok ( ({last_action = ((fun (c : chain) -> c.(1).(0)) chain); next_step = {last_action = state.last_action; next_step = Buyer_confirm; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.next_step; seller = {last_action = state.last_action; next_step = Buyer_confirm; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller; buyer = {last_action = state.last_action; next_step = Buyer_confirm; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.buyer; seller_withdrawable = {last_action = state.last_action; next_step = Buyer_confirm; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller_withdrawable; buyer_withdrawable = {last_action = state.last_action; next_step = Buyer_confirm; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.buyer_withdrawable}, [])))
 | Err (e0) -> (Err (e0)))
 | Err (e0) -> (Err (e0)))
 | Err (e0) -> (Err (e0)))
 | Buyer_confirm  -> (Err (default_error))
 | Withdrawals  -> (Err (default_error))
 | No_next_step  -> (Err (default_error)))
 | Confirm_item_received  -> (match state.next_step with 
Buyer_commit  -> (Err (default_error))
 | Buyer_confirm  -> (let item_price = divTez ((fun (c : cctx) -> c.(1).(1).(0)) ctx) 4tz in 
match if eq_addr ((fun (c : cctx) -> c.(1).(0)) ctx) state.buyer then Ok (()) else Err (default_error) with 
Ok (t0) -> (match if eqTez ((fun (c : cctx) -> c.(1).(1).(1)) ctx) 0tz then Ok (()) else Err (default_error) with 
Ok (t1) -> (let new_state = {last_action = {last_action = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.last_action; next_step = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.next_step; seller = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller; buyer = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.buyer; seller_withdrawable = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller_withdrawable; buyer_withdrawable = item_price}.last_action; next_step = {last_action = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.last_action; next_step = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.next_step; seller = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller; buyer = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.buyer; seller_withdrawable = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller_withdrawable; buyer_withdrawable = item_price}.next_step; seller = {last_action = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.last_action; next_step = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.next_step; seller = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller; buyer = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.buyer; seller_withdrawable = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller_withdrawable; buyer_withdrawable = item_price}.seller; buyer = {last_action = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.last_action; next_step = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.next_step; seller = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller; buyer = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.buyer; seller_withdrawable = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller_withdrawable; buyer_withdrawable = item_price}.buyer; seller_withdrawable = (multTez item_price 3tz); buyer_withdrawable = {last_action = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.last_action; next_step = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.next_step; seller = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller; buyer = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.buyer; seller_withdrawable = {last_action = state.last_action; next_step = Withdrawals; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}.seller_withdrawable; buyer_withdrawable = item_price}.buyer_withdrawable} in 
Ok ( (new_state, [])))
 | Err (e0) -> (Err (e0)))
 | Err (e0) -> (Err (e0)))
 | Withdrawals  -> (Err (default_error))
 | No_next_step  -> (Err (default_error)))
 | Withdraw  -> (match state.next_step with 
Buyer_commit  -> (match if eqTez ((fun (c : cctx) -> c.(1).(1).(1)) ctx) 0tz then Ok (()) else Err (default_error) with 
Ok (t0) -> (match if ltNat (addNat state.last_action 50p) ((fun (c : chain) -> c.(1).(0)) chain) then Err (default_error) else Ok (()) with 
Ok (t1) -> (match if eq_addr ((fun (c : cctx) -> c.(1).(0)) ctx) state.seller then Ok (()) else Err (default_error) with 
Ok (t2) -> (let balance0 = (fun (c : cctx) -> c.(1).(1).(0)) ctx in 
Ok ( ({last_action = state.last_action; next_step = No_next_step; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = state.buyer_withdrawable}, (Contract.call state.seller balance0 default () :: []))))
 | Err (e0) -> (Err (e0)))
 | Err (e0) -> (Err (e0)))
 | Err (e0) -> (Err (e0)))
 | Buyer_confirm  -> (Err (default_error))
 | Withdrawals  -> (match if eqTez ((fun (c : cctx) -> c.(1).(1).(1)) ctx) 0tz then Ok (()) else Err (default_error) with 
Ok (t0) -> (let from0 = (fun (c : cctx) -> c.(1).(0)) ctx in 
match if eq_addr from0 state.buyer then Ok ( (state.buyer_withdrawable, {last_action = state.last_action; next_step = state.next_step; seller = state.seller; buyer = state.buyer; seller_withdrawable = state.seller_withdrawable; buyer_withdrawable = 0tz})) else if eq_addr from0 state.seller then Ok ( (state.seller_withdrawable, {last_action = state.last_action; next_step = state.next_step; seller = state.seller; buyer = state.buyer; seller_withdrawable = 0tz; buyer_withdrawable = state.buyer_withdrawable})) else Err (default_error) with 
Ok (t1) -> (let (new_state0,to_pay0) = t1 in (match if gtTez new_state0 0tz then Ok (()) else Err (default_error) with 
Ok (t2) -> (let new_state1 = if andb (eqTez to_pay0.buyer_withdrawable 0tz) (eqTez to_pay0.seller_withdrawable 0tz) then {last_action = to_pay0.last_action; next_step = No_next_step; seller = to_pay0.seller; buyer = to_pay0.buyer; seller_withdrawable = to_pay0.seller_withdrawable; buyer_withdrawable = to_pay0.buyer_withdrawable} else to_pay0 in 
Ok ( (new_state1, (Contract.call ((fun (c : cctx) -> c.(1).(0)) ctx) new_state0 default () :: []))))
 | Err (e0) -> (Err (e0))))
 | Err (e0) -> (Err (e0)))
 | Err (e0) -> (Err (e0)))
 | No_next_step  -> (Err (default_error))))
 | None  -> (Err (default_error))

let liquidity_escrow_receive (m : ((msg * cctx) * chain)) (s : state) = match receive m.(1) m.(0).(1) s (Some (m.(0).(0))) with 
Ok (t0) -> (let (acts0,s0) = t0 in (Ok ( (s0, acts0))))
 | Err (e0) -> (Err (e0))

let%init storage (s : (setup * chain)) = 
let evenTez (i : tez) = match i/2tz with | Some (_, r) -> r=0tz | None -> false in
let eqTez (a : tez ) (b : tez ) = a = b in
let eq_addr (a1 : address) (a2 : address) = a1 = a2 in
let andb (a : bool ) (b : bool ) = a & b in
let default_error = 1 in

let inner (cctx : cctx) (s : (setup * chain)) = let seller = (fun (c : cctx) -> c.(1).(0)) cctx in 
let buyer = s.(0) in 
match if eq_addr buyer seller then Err (default_error) else Ok (()) with 
Ok (t0) -> (match if eqTez ((fun (c : cctx) -> c.(1).(1).(1)) cctx) 0tz then Err (default_error) else Ok (()) with 
Ok (t1) -> (match if evenTez ((fun (c : cctx) -> c.(1).(1).(1)) cctx) then Ok (()) else Err (default_error) with 
Ok (t2) -> (Ok ({last_action = ((fun (c : chain) -> c.(1).(0)) s.(1)); next_step = Buyer_commit; seller = seller; buyer = buyer; seller_withdrawable = 0tz; buyer_withdrawable = 0tz}))
 | Err (e0) -> (Err (e0)))
 | Err (e0) -> (Err (e0)))
 | Err (e0) -> (Err (e0)) in
let ctx = (Current.time (),
   (Current.sender (),
   (Current.amount (),
    Current.balance ()))) in
match (inner ctx s) with Ok v -> v | Err e -> failwith e

type storage = state
let wrapper param (st : storage) = match liquidity_escrow_receive param st with| Ok v -> v| Err e -> failwith e
let%entry main param st = wrapper param st
(*END*)
