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
let[@inline] ltTez (a : tez ) (b : tez ) =  a < b
let[@inline] gtTez (a : tez ) (b : tez ) =  a > b
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

type  tokenValue = int

type msg = 
Transfer of address * tokenValue
| Transfer_from of address * address * tokenValue
| Approve of address * tokenValue


type setup = {
owner : address;
init_amount : tokenValue
}

type state = {
total_supply : tokenValue;
balances : ( (address, tokenValue) map);
allowances : ( (address, ( (address, tokenValue) map)) map)
}

let increment_balance (m : ( (address, tokenValue) map)) (addr : address) (inc : tokenValue) = match Map.find addr m with 
Some (old) -> (Map.add addr (addInt old inc) m)
 | None  -> (Map.add addr inc m)

let try_transfer (from0 : address) (to0 : address) (amount : tokenValue) (state : state) = let from_balance = match Map.find from0 state.balances with 
Some (v) -> v
 | None  -> 0 in 
if ltInt from_balance amount then None else let new_balances = Map.add from0 (subInt from_balance amount) state.balances in 
let new_balances0 = increment_balance new_balances to0 amount in 
Some ({total_supply = state.total_supply; balances = new_balances0; allowances = state.allowances})

let try_transfer_from (delegate : address) (from0 : address) (to0 : address) (amount : tokenValue) (state : state) = match Map.find from0 state.allowances with 
Some (val0) -> (match Map.find delegate val0 with 
Some (val1) -> (let from_balance = match Map.find from0 state.balances with 
Some (v) -> v
 | None  -> 0 in 
if orb (ltInt val1 amount) (ltInt from_balance amount) then None else let new_allowances = Map.add delegate (subInt val1 amount) val0 in 
let new_balances = Map.add from0 (subInt from_balance amount) state.balances in 
let new_balances0 = increment_balance new_balances to0 amount in 
Some ({total_supply = {total_supply = state.total_supply; balances = new_balances0; allowances = state.allowances}.total_supply; balances = {total_supply = state.total_supply; balances = new_balances0; allowances = state.allowances}.balances; allowances = (Map.add from0 new_allowances {total_supply = state.total_supply; balances = new_balances0; allowances = state.allowances}.allowances)}))
 | None  -> None)
 | None  -> None

let try_approve (caller : address) (delegate : address) (amount : tokenValue) (state : state) = match Map.find caller state.allowances with 
Some (caller_allowances) -> (Some ({total_supply = state.total_supply; balances = state.balances; allowances = (Map.add caller (Map.add delegate amount caller_allowances) state.allowances)}))
 | None  -> (Some ({total_supply = state.total_supply; balances = state.balances; allowances = (Map.add caller (Map.add delegate amount (Map [])) state.allowances)}))

let test_receive (ctx : (timestamp * (address * (tez * tez)))) (state : state) (maybe_msg : ( (msg) option)) = let sender = (fun x -> x.(1).(0)) ctx in 
let without_actions = fun o -> match o with 
Some (a) -> (Some ( ([], a)))
 | None  -> None in 
match maybe_msg with 
Some (m) -> (match m with 
Transfer (to_addr, amountt) -> (without_actions (try_transfer sender to_addr amountt state))
 | Transfer_from (from0, to_addr, amountt) -> (without_actions (try_transfer_from sender from0 to_addr amountt state))
 | Approve (delegate, amount) -> (without_actions (try_approve sender delegate amount state)))
 | None  -> None

let receive_wrapper (params : ((timestamp * (address * (tez * tez))) * ( (msg) option))) (st : state) = test_receive params.(0) st params.(1)

let%init storage (setup : setup) = 
let inner (ctx : (timestamp * (address * (tez * tez)))) (setup : setup) = let ctx_ = ctx in 
Some ({total_supply = setup.init_amount; balances = (Map.add setup.owner setup.init_amount (Map [])); allowances = (Map [])}) in
let ctx = (Current.time (),
   (Current.sender (),
   (Current.amount (),
    Current.balance ()))) in
match (inner ctx setup) with Some v -> v | None -> failwith ()

type storage = state
let wrapper param (st : storage) = match receive_wrapper ((Current.time (),
   (Current.sender (),
   (Current.amount (),
    Current.balance ()))), param) st with| Some v -> v| None -> failwith ()
let%entry main param st = wrapper param st
(*END*)
