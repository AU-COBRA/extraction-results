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
let[@inline] leTez (a : tez) (b : tez) = a <= b
let[@inline] ltTez (a : tez) (b : tez) = a < b
let[@inline] gtTez (a : tez) (b : tez) = a > b
let[@inline] eqTez (a : tez) (b : tez) = a = b
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

let[@inline] andb (a : bool) (b : bool) = a & b
let[@inline] orb (a : bool) (b : bool) = a || b

let[@inline] eqb_time (a1 : timestamp) (a2 : timestamp) = a1 = a2
let[@inline] leb_time (a1 : timestamp) (a2 : timestamp) = a1 <= a2
let[@inline] ltb_time (a1 : timestamp) (a2 : timestamp) = a1 < a2

let[@inline] eq_addr (a1 : address) (a2 : address) = a1 = a2

type 'a addrMap = (address, 'a) map

type ('t,'e) result =
  Ok of 't
| Err of 'e
type storage = ((timestamp * (tez * address)) * ((address,tez) map * bool))

type msg_coq = 
Donate_coq
| GetFunds_coq
| Claim_coq


type  simpleCallCtx = (timestamp * (address * (tez * tez)))

type  error = nat

let default_error  = 1p

let update_contribs (f_st : ((timestamp * (tez * address)) * ((address,tez) map * bool))) (cs : (address,tez) map) =  (f_st.(0),  (cs, f_st.(1).(1)))

let maybe_bind_unit (o : ( (unit) option)) (b : ( ((( (operation) list) * ((timestamp * (tez * address)) * ((address,tez) map * bool)))) option)) = match o with 
Some (_0) -> b
 | None  -> None

let validate (tx_amount : tez) = if eqTez 0DUN tx_amount then Some (()) else None

let set_done (f_st : ((timestamp * (tez * address)) * ((address,tez) map * bool))) =  (f_st.(0),  (f_st.(1).(0), true))

let receive (m : msg_coq) (s : ((timestamp * (tez * address)) * ((address,tez) map * bool))) (ctx : (timestamp * (address * (tez * tez)))) = match m with 
Donate_coq  -> (if leb_time ctx.(0) s.(0).(0) then match Map.find ctx.(1).(0) s.(1).(0) with 
Some (v0) -> (Some ( ([], (update_contribs s (Map.add ctx.(1).(0) (addTez v0 ctx.(1).(1).(0)) s.(1).(0))))))
 | None  -> (Some ( ([], (update_contribs s (Map.add ctx.(1).(0) ctx.(1).(1).(0) s.(1).(0)))))) else None)
 | GetFunds_coq  -> (if andb (andb (eq_addr s.(0).(1).(1) ctx.(1).(0)) (ltb_time s.(0).(0) ctx.(0))) (leTez s.(0).(1).(0) ctx.(1).(1).(1)) then maybe_bind_unit (validate ctx.(1).(1).(0)) (Some ( ((Contract.call ctx.(1).(0) ctx.(1).(1).(1) default () :: []), (set_done s)))) else None)
 | Claim_coq  -> (if andb (andb (ltb_time s.(0).(0) ctx.(0)) (ltTez ctx.(1).(1).(1) s.(0).(1).(0))) (not s.(1).(1)) then match Map.find ctx.(1).(0) s.(1).(0) with 
Some (v0) -> (maybe_bind_unit (validate ctx.(1).(1).(0)) (Some ( ((Contract.call ctx.(1).(0) v0 default () :: []), (update_contribs s (Map.add ctx.(1).(0) 0DUN s.(1).(0)))))))
 | None  -> None else None)

let crowdfunding_receive (params : ((timestamp * (address * (tez * tez))) * msg_coq)) (st : ((timestamp * (tez * address)) * ((address,tez) map * bool))) = match receive params.(1) st params.(0) with 
Some (v0) -> (Ok (v0))
 | None  -> (Err (default_error))

let%init storage (setup : (timestamp * (tez * address))) = let fst (p : 'a * 'b) : 'a = p.(0) in
let snd (p : 'a * 'b) : 'b = p.(1) in
let eqTez (a : tez) (b : tez) = a = b in
let default_error = 1 in
let inner (ctx : simpleCallCtx) (setup : (timestamp * (tez * address))) = if eqTez ctx.(1).(1).(0) 0DUN then Ok ( (setup,  (Map [], false))) else Err (default_error) in
let ctx = (Current.time (),
   (Current.sender (),
   (Current.amount (),
    Current.balance ()))) in
match (inner ctx setup) with Ok v -> v | Err e -> failwith e

let wrapper param (st : storage) = match crowdfunding_receive param st with| Ok v -> v| Err e -> failwith e
let%entry main param st = wrapper param st
(*END*)
