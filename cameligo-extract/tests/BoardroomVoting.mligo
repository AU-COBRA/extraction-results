
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
let unsafe_int_to_nat (n : int) = abs(n)
  let predN (n : nat) = unsafe_int_to_nat (n - 1n)
  let mod_pow (a : int) (e : int) (p : int) : int = failwith ("unimplemented")
  let egcd (a : int) (p : int) : int * int = failwith ("unimplemented")
  
  let nth  = let rec nth  (n, l, default : nat * int list * int) : int = 
  if n = 0n then (match l with 
  []  -> default
   | x :: r -> x)
  else let m = predN n in (match l with 
  []  -> default
   | x :: t -> (nth (m, t, default)))
   in fun (n:nat) (l:int list) (default:int) -> nth (n, l, default)
   
   
  let prod (l : int list) =
    List.fold (fun (a, b: int*int) -> multInt a b) l 1
  
  let firstn (n : nat) (l : int list) : int list =
    let (_,r) = List.fold_left (fun ((n, a),b : (nat * int list) * int) ->
        if n = 0n then (0n, a)
        else (predN n, b :: a)) (n,([] : int list)) l in
   r
  
  let skipn  = let rec skipn  (n, l : nat * int list) : int list = 
   if n = 0n then l
    else let n0 = predN n in (match l with 
    | []  -> ([]:int list)
    | a :: l0 -> (skipn (n0, l0 : nat * int list)))
    in fun (n : nat) (l : int list) -> skipn (n, l : nat * int list)
let hash_func (l :  (nat) list) = addN 1n (List.fold_left (fun (a, p : nat * nat) -> Bitwise.xor p a) 1n l)

type setup = {
eligible_voters :  (address, unit) map;
finish_registration_by : nat;
finish_commit_by :  (nat) option;
finish_vote_by : nat;
registration_deposit : tez
}

type setupWchain = (setup * chain)

type a = int

type voterInfo = {
voter_index : nat;
vote_hash : nat;
public_vote : a
}

type state = {
owner : address;
registered_voters :  (address, voterInfo) map;
public_keys :  (a) list;
setup : setup;
tally :  (nat) option
}

type voteProof = ((((((((int * a) * a) * a) * a) * int) * int) * int) * int)

type msg = 
  Signup of (a * (a * int))
| Commit_to_vote of (nat)
| Submit_vote of (a * voteProof)
| Tally_votes


let init (chain : chain) (ctx : cctx) (setup : setup) = match match  ( (chain, ctx), Tezos.sender) with 
 (p, res) -> (match p with 
 (chain0, ctx0) ->  ( ( (chain0, ctx0), setup), (Some (res)))) with 
 (p, res) -> (match p with 
 (p0, setup0) -> (match p0 with 
 (chain0, ctx0) -> (match res with 
Some (res0) -> (match  ( ( (chain0, ctx0), setup0), (Some (setup0))) with 
 (p1, res1) -> (match p1 with 
 (p00, setup1) -> (match p00 with 
 (chain1, ctx1) -> (match res1 with 
Some (res2) -> (match  ( ( (chain1, ctx1), setup1), (if ltbN res2.finish_registration_by res2.finish_vote_by then Some (()) else (None: (unit) option))) with 
 (p2, res3) -> (match p2 with 
 (p01, setup2) -> (match p01 with 
 (chain2, ctx2) -> (match res3 with 
Some (res4) ->  ( ( (chain2, ctx2), setup2), (Some (({owner = res0; registered_voters = (Map.empty:  (address, voterInfo) map); public_keys = ([]: (a) list); setup = res2; tally = (None: (nat) option)}: state))))
 | None  ->  ( ( (chain2, ctx2), setup2), (None: (state) option))))))
 | None  ->  ( ( (chain1, ctx1), setup1), (None: (state) option))))))
 | None  ->  ( ( (chain0, ctx0), setup0), (None: (state) option)))))

let prime  = 201697267445741585806196628073

let order  = prime

let generator  = 3

let hash_sk_data (gv : a) (pk : a) (i : nat) = hash_func ((unsafe_int_to_nat generator) :: ((unsafe_int_to_nat gv) :: ((unsafe_int_to_nat pk) :: (( i) :: ([]: (nat) list)))))

let elmeqb (a : a) (b : a) = eqInt (modInt a prime) (modInt b prime)

let mul_p (a : int) (b : int) = modInt (multInt a b) prime

let mod_inv (a : int) (p : int) = let x = egcd a p in 
modInt x.0 p

let pow_p (a : int) (e : int) = mod_pow a e prime

let verify_secret_key_proof (pk : a) (i : nat) (proof : (a * int)) = match proof with 
 (gv, r) -> (let z = int ((hash_sk_data gv pk i)) in 
elmeqb gv (mul_p (pow_p generator r) (pow_p pk z)))

let handle_signup (pk : a) (prf : (a * int)) (state : state) (caller : address) (cur_slot : nat) (chain : chain) (ctx : cctx) (state0 : state) (msg :  (msg) option) (acts :  (operation) list) = match  ( ( ( ( (chain, ctx), state0), msg), acts), (if ltbN state.setup.finish_registration_by cur_slot then (None: (unit) option) else Some (()))) with 
 (p, res) -> (match p with 
 (p0, acts0) -> (match p0 with 
 (p1, msg0) -> (match p1 with 
 (p2, state1) -> (match p2 with 
 (chain0, ctx0) -> (match res with 
Some (res0) -> (match  ( ( ( ( (chain0, ctx0), state1), msg0), acts0), (match Map.find_opt caller state.setup.eligible_voters with 
Some (a) -> (Some (()))
 | None  -> (None: (unit) option))) with 
 (p3, res1) -> (match p3 with 
 (p00, acts1) -> (match p00 with 
 (p10, msg1) -> (match p10 with 
 (p20, state2) -> (match p20 with 
 (chain1, ctx1) -> (match res1 with 
Some (res2) -> (match  ( ( ( ( (chain1, ctx1), state2), msg1), acts1), (match Map.find_opt caller state.registered_voters with 
Some (a) -> (None: (unit) option)
 | None  -> (Some (())))) with 
 (p4, res3) -> (match p4 with 
 (p01, acts2) -> (match p01 with 
 (p11, msg2) -> (match p11 with 
 (p21, state3) -> (match p21 with 
 (chain2, ctx2) -> (match res3 with 
Some (res4) -> (match match  ( (chain2, ctx2), Tezos.amount) with 
 (p5, res5) -> (match p5 with 
 (chain3, ctx3) ->  ( ( ( ( (chain3, ctx3), state3), msg2), acts2), (Some (res5)))) with 
 (p5, res5) -> (match p5 with 
 (p02, acts3) -> (match p02 with 
 (p12, msg3) -> (match p12 with 
 (p22, state4) -> (match p22 with 
 (chain3, ctx3) -> (match res5 with 
Some (res6) -> (match  ( ( ( ( (chain3, ctx3), state4), msg3), acts3), (if eqTez res6 state.setup.registration_deposit then Some (()) else (None: (unit) option))) with 
 (p6, res7) -> (match p6 with 
 (p03, acts4) -> (match p03 with 
 (p13, msg4) -> (match p13 with 
 (p23, state5) -> (match p23 with 
 (chain4, ctx4) -> (match res7 with 
Some (res8) -> (match  ( ( ( ( (chain4, ctx4), state5), msg4), acts4), (if ltInt (int (List.length state.public_keys)) (subInt order 2) then Some (()) else (None: (unit) option))) with 
 (p7, res9) -> (match p7 with 
 (p04, acts5) -> (match p04 with 
 (p14, msg5) -> (match p14 with 
 (p24, state6) -> (match p24 with 
 (chain5, ctx5) -> (match res9 with 
Some (res) -> ((let index = List.length state.public_keys in 
fun (chain6 : chain) -> fun (ctx6 : cctx) -> fun (state7 : state) -> fun (msg6 :  (msg) option) -> fun (acts6 :  (operation) list) -> match  ( ( ( ( (chain6, ctx6), state7), msg6), acts6), (if verify_secret_key_proof pk index prf then Some (()) else (None: (unit) option))) with 
 (p8, res) -> (match p8 with 
 (p05, acts7) -> (match p05 with 
 (p15, msg7) -> (match p15 with 
 (p25, state8) -> (match p25 with 
 (chain7, ctx7) -> (match res with 
Some (res) -> ((let inf = ({voter_index = index; vote_hash = 1n; public_vote = 0}: voterInfo) in 
let new_state = ({owner = state.owner; registered_voters = (Map.add caller inf state.registered_voters); public_keys = (List.fold_left (fun (acc, e : (int list) * int) -> e :: acc) state.public_keys (pk :: ([]: (a) list))); setup = state.setup; tally = state.tally}: state) in 
fun (chain8 : chain) -> fun (ctx8 : cctx) -> fun (state9 : state) -> fun (msg8 :  (msg) option) -> fun (acts8 :  (operation) list) ->  ( ( ( ( (chain8, ctx8), state9), msg8), acts8), (Some (new_state)))) chain7 ctx7 state8 msg7 acts7)
 | None  ->  ( ( ( ( (chain7, ctx7), state8), msg7), acts7), (None: (state) option)))))))) chain5 ctx5 state6 msg5 acts5)
 | None  ->  ( ( ( ( (chain5, ctx5), state6), msg5), acts5), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain4, ctx4), state5), msg4), acts4), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain3, ctx3), state4), msg3), acts3), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain2, ctx2), state3), msg2), acts2), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain1, ctx1), state2), msg1), acts1), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain0, ctx0), state1), msg0), acts0), (None: (state) option)))))))

let voter_index (v : voterInfo) = v.voter_index

let vote_hash (v : voterInfo) = v.vote_hash

let public_vote (v : voterInfo) = v.public_vote

let owner (s : state) = s.owner

let registered_voters (s : state) = s.registered_voters

let public_keys (s : state) = s.public_keys

let setup (s : state) = s.setup

let tally (s : state) = s.tally

let handle_commit_to_vote (hash_ : nat) (state : state) (caller : address) (cur_slot : nat) (chain : chain) (ctx : cctx) (state0 : state) (msg :  (msg) option) (acts :  (operation) list) = match  ( ( ( ( (chain, ctx), state0), msg), acts), state.setup.finish_commit_by) with 
 (p, res) -> (match p with 
 (p0, acts0) -> (match p0 with 
 (p1, msg0) -> (match p1 with 
 (p2, state1) -> (match p2 with 
 (chain0, ctx0) -> (match res with 
Some (res0) -> (match  ( ( ( ( (chain0, ctx0), state1), msg0), acts0), (if ltbN res0 cur_slot then (None: (unit) option) else Some (()))) with 
 (p3, res1) -> (match p3 with 
 (p00, acts1) -> (match p00 with 
 (p10, msg1) -> (match p10 with 
 (p20, state2) -> (match p20 with 
 (chain1, ctx1) -> (match res1 with 
Some (res2) -> (match  ( ( ( ( (chain1, ctx1), state2), msg1), acts1), (Map.find_opt caller state.registered_voters)) with 
 (p4, res3) -> (match p4 with 
 (p01, acts2) -> (match p01 with 
 (p11, msg2) -> (match p11 with 
 (p21, state3) -> (match p21 with 
 (chain2, ctx2) -> (match res3 with 
Some (res4) -> ((let inf = ({voter_index = (voter_index res4); vote_hash = hash_; public_vote = (public_vote res4)}: voterInfo) in 
fun (chain3 : chain) -> fun (ctx3 : cctx) -> fun (state4 : state) -> fun (msg3 :  (msg) option) -> fun (acts3 :  (operation) list) ->  ( ( ( ( (chain3, ctx3), state4), msg3), acts3), (Some (({owner = (owner state); registered_voters = (Map.add caller inf (registered_voters state)); public_keys = (public_keys state); setup = (setup state); tally = (tally state)}: state))))) chain2 ctx2 state3 msg2 acts2)
 | None  ->  ( ( ( ( (chain2, ctx2), state3), msg2), acts2), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain1, ctx1), state2), msg1), acts1), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain0, ctx0), state1), msg0), acts0), (None: (state) option)))))))

let hash_sv_data (i : nat) (pk : a) (rk : a) (a1 : a) (b1 : a) (a2 : a) (b2 : a) = hash_func (( i) :: (List.map unsafe_int_to_nat (pk :: (rk :: (a1 :: (b1 :: (a2 :: (b2 :: ([]: (a) list)))))))))

let inv_p (a : int) = mod_inv a prime

let verify_secret_vote_proof (pk : a) (rk : a) (pv : a) (i : nat) (proof : voteProof) = match proof with 
 (p, r2) -> (match p with 
 (p0, r1) -> (match p0 with 
 (p1, d2) -> (match p1 with 
 (p2, d1) -> (match p2 with 
 (p3, b2) -> (match p3 with 
 (p4, a2) -> (match p4 with 
 (p5, b1) -> (match p5 with 
 (w, a1) -> (let c = hash_sv_data i pk rk a1 b1 a2 b2 in 
andb (andb (andb (andb (eqInt (int (c)) (addInt d1 d2)) (elmeqb a1 (mul_p (pow_p generator r1) (pow_p pk d1)))) (elmeqb b1 (mul_p (pow_p rk r1) (pow_p pv d1)))) (elmeqb a2 (mul_p (pow_p generator r2) (pow_p pk d2)))) (elmeqb b2 (mul_p (pow_p rk r2) (pow_p (multInt pv (inv_p generator)) d2)))))))))))

let reconstructed_key (pks :  (a) list) (n : nat) = let lprod = prod (firstn n pks) in 
let rprod = inv_p (prod (skipn (1n + (n)) pks)) in 
mul_p lprod rprod

let handle_submit_vote (v : a) (proof : voteProof) (state : state) (caller : address) (cur_slot : nat) (chain : chain) (ctx : cctx) (state0 : state) (msg :  (msg) option) (acts :  (operation) list) = match  ( ( ( ( (chain, ctx), state0), msg), acts), (if ltbN state.setup.finish_vote_by cur_slot then (None: (unit) option) else Some (()))) with 
 (p, res) -> (match p with 
 (p0, acts0) -> (match p0 with 
 (p1, msg0) -> (match p1 with 
 (p2, state1) -> (match p2 with 
 (chain0, ctx0) -> (match res with 
Some (res0) -> (match  ( ( ( ( (chain0, ctx0), state1), msg0), acts0), (Map.find_opt caller state.registered_voters)) with 
 (p3, res1) -> (match p3 with 
 (p00, acts1) -> (match p00 with 
 (p10, msg1) -> (match p10 with 
 (p20, state2) -> (match p20 with 
 (chain1, ctx1) -> (match res1 with 
Some (res2) -> (match  ( ( ( ( (chain1, ctx1), state2), msg1), acts1), (match state.setup.finish_commit_by with 
Some (a) -> (if eqN (hash_func ((unsafe_int_to_nat v) :: ([]: (nat) list))) res2.vote_hash then Some (()) else (None: (unit) option))
 | None  -> (Some (())))) with 
 (p4, res3) -> (match p4 with 
 (p01, acts2) -> (match p01 with 
 (p11, msg2) -> (match p11 with 
 (p21, state3) -> (match p21 with 
 (chain2, ctx2) -> (match res3 with 
Some (res4) -> (match  ( ( ( ( (chain2, ctx2), state3), msg2), acts2), (if verify_secret_vote_proof (nth res2.voter_index state.public_keys 0) (reconstructed_key state.public_keys res2.voter_index) v res2.voter_index proof then Some (()) else (None: (unit) option))) with 
 (p5, res5) -> (match p5 with 
 (p02, acts3) -> (match p02 with 
 (p12, msg3) -> (match p12 with 
 (p22, state4) -> (match p22 with 
 (chain3, ctx3) -> (match res5 with 
Some (res6) -> ((let inf = ({voter_index = (voter_index res2); vote_hash = (vote_hash res2); public_vote = v}: voterInfo) in 
fun (chain4 : chain) -> fun (ctx4 : cctx) -> fun (state5 : state) -> fun (msg4 :  (msg) option) -> fun (acts4 :  (operation) list) ->  ( ( ( ( (chain4, ctx4), state5), msg4), acts4), (Some (({owner = (owner state); registered_voters = (Map.add caller inf (registered_voters state)); public_keys = (public_keys state); setup = (setup state); tally = (tally state)}: state))))) chain3 ctx3 state4 msg3 acts3)
 | None  ->  ( ( ( ( (chain3, ctx3), state4), msg3), acts3), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain2, ctx2), state3), msg2), acts2), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain1, ctx1), state2), msg1), acts1), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain0, ctx0), state1), msg0), acts0), (None: (state) option)))))))

let handle_tally_votes (state : state) (cur_slot : nat) (chain : chain) (ctx : cctx) (state0 : state) (msg :  (msg) option) (acts :  (operation) list) = match  ( ( ( ( (chain, ctx), state0), msg), acts), (if ltbN cur_slot state.setup.finish_vote_by then (None: (unit) option) else Some (()))) with 
 (p, res) -> (match p with 
 (p0, acts0) -> (match p0 with 
 (p1, msg0) -> (match p1 with 
 (p2, state1) -> (match p2 with 
 (chain0, ctx0) -> (match res with 
Some (res0) -> (match  ( ( ( ( (chain0, ctx0), state1), msg0), acts0), (match state.tally with 
Some (n) -> (None: (unit) option)
 | None  -> (Some (())))) with 
 (p3, res1) -> (match p3 with 
 (p00, acts1) -> (match p00 with 
 (p10, msg1) -> (match p10 with 
 (p20, state2) -> (match p20 with 
 (chain1, ctx1) -> (match res1 with 
Some (res2) -> ((let voters = (fun (v:(address, voterInfo) map) ->
    Map.fold (fun (acc, (_,info) : voterInfo list * (address * voterInfo)) -> info :: acc)
    v ([]: voterInfo list)) state.registered_voters in 
fun (chain2 : chain) -> fun (ctx2 : cctx) -> fun (state3 : state) -> fun (msg2 :  (msg) option) -> fun (acts2 :  (operation) list) -> match  ( ( ( ( (chain2, ctx2), state3), msg2), acts2), (if (let existsb (f : voterInfo -> bool) = let rec existsb  (l: voterInfo list) : bool = 
  match l with 
  []  -> false
  | a :: l0 -> (if (f a) then true else (existsb (l0)))
  in fun (l: voterInfo list) -> existsb (l) in existsb) (fun (vi : voterInfo) -> if elmeqb vi.public_vote 0 then true else false) voters then (None: (unit) option) else Some (()))) with 
 (p4, res3) -> (match p4 with 
 (p01, acts3) -> (match p01 with 
 (p11, msg3) -> (match p11 with 
 (p21, state4) -> (match p21 with 
 (chain3, ctx3) -> (match res3 with 
Some (res4) -> ((let votes = List.map public_vote voters in 
fun (chain4 : chain) -> fun (ctx4 : cctx) -> fun (state5 : state) -> fun (msg4 :  (msg) option) -> fun (acts4 :  (operation) list) -> match  ( ( ( ( (chain4, ctx4), state5), msg4), acts4), ((fun (votes :  (a) list) -> 
  let rec bruteforce_tally_aux  (n, votes_product : nat * a) : nat option = 
    if elmeqb (pow_p generator (int n)) votes_product then 
        Some (n) 
    else if n = 0n then 
      None
    else
      let n0 = n - 1n in
        (bruteforce_tally_aux (unsafe_int_to_nat n0, votes_product))
  in bruteforce_tally_aux ((List.length votes), (prod votes))) votes)) with 
 (p5, res5) -> (match p5 with 
 (p02, acts5) -> (match p02 with 
 (p12, msg5) -> (match p12 with 
 (p22, state6) -> (match p22 with 
 (chain5, ctx5) -> (match res5 with 
Some (res6) ->  ( ( ( ( (chain5, ctx5), state6), msg5), acts5), (Some (({owner = (owner state); registered_voters = (registered_voters state); public_keys = (public_keys state); setup = (setup state); tally = (Some (res6))}: state))))
 | None  ->  ( ( ( ( (chain5, ctx5), state6), msg5), acts5), (None: (state) option)))))))) chain3 ctx3 state4 msg3 acts3)
 | None  ->  ( ( ( ( (chain3, ctx3), state4), msg3), acts3), (None: (state) option)))))))) chain1 ctx1 state2 msg1 acts1)
 | None  ->  ( ( ( ( (chain1, ctx1), state2), msg1), acts1), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain0, ctx0), state1), msg0), acts0), (None: (state) option)))))))

let receive (chain : chain) (ctx : cctx) (state : state) (msg :  (msg) option) (acts :  (operation) list) = match  ( ( ( ( (chain, ctx), state), msg), acts), (Some (state))) with 
 (p, res) -> (match p with 
 (p0, acts0) -> (match p0 with 
 (p1, msg0) -> (match p1 with 
 (p2, state0) -> (match p2 with 
 (chain0, ctx0) -> (match res with 
Some (res0) -> (match match  ( (chain0, ctx0), Tezos.sender) with 
 (p3, res1) -> (match p3 with 
 (chain1, ctx1) ->  ( ( ( ( (chain1, ctx1), state0), msg0), acts0), (Some (res1)))) with 
 (p3, res1) -> (match p3 with 
 (p00, acts1) -> (match p00 with 
 (p10, msg1) -> (match p10 with 
 (p20, state1) -> (match p20 with 
 (chain1, ctx1) -> (match res1 with 
Some (res2) -> (match match  ( (chain1, ctx1), chain1.current_slot) with 
 (p4, res3) -> (match p4 with 
 (chain2, ctx2) ->  ( ( ( ( (chain2, ctx2), state1), msg1), acts1), (Some (res3)))) with 
 (p4, res3) -> (match p4 with 
 (p01, acts2) -> (match p01 with 
 (p11, msg2) -> (match p11 with 
 (p21, state2) -> (match p21 with 
 (chain2, ctx2) -> (match res3 with 
Some (res4) -> (match match  ( ( ( ( (chain2, ctx2), state2), msg2), acts2), (Some (msg2))) with 
 (p5, res5) -> (match p5 with 
 (p02, acts3) -> (match p02 with 
 (p12, msg3) -> (match p12 with 
 (p22, state3) -> (match p22 with 
 (chain3, ctx3) -> (match res5 with 
Some (res6) ->  ( ( ( ( (chain3, ctx3), state3), msg3), acts3), res6)
 | None  ->  ( ( ( ( (chain3, ctx3), state3), msg3), acts3), (None: (msg) option))))))) with 
 (p5, res5) -> (match p5 with 
 (p02, acts3) -> (match p02 with 
 (p12, msg3) -> (match p12 with 
 (p22, state3) -> (match p22 with 
 (chain3, ctx3) -> (match res5 with 
Some (res6) -> ((match res6 with 
Signup (pk, prf) -> (handle_signup pk prf res0 res2 res4)
 | Commit_to_vote (hash_) -> (handle_commit_to_vote hash_ res0 res2 res4)
 | Submit_vote (v, proof) -> (handle_submit_vote v proof res0 res2 res4)
 | Tally_votes  -> (handle_tally_votes res0 res4)) chain3 ctx3 state3 msg3 acts3)
 | None  ->  ( ( ( ( (chain3, ctx3), state3), msg3), acts3), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain2, ctx2), state2), msg2), acts2), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain1, ctx1), state1), msg1), acts1), (None: (state) option))))))))
 | None  ->  ( ( ( ( (chain0, ctx0), state0), msg0), acts0), (None: (state) option)))))))

let receive_wrapper (c : chain) (ctx : cctx) (st : state) (msg :  (msg) option) = match match receive c ctx st msg ([]: (operation) list) with 
 (p, result) -> (match p with 
 (p0, acts) -> (match p0 with 
 (p1, o) -> (match p1 with 
 (p2, y) -> (match p2 with 
 (c0, c00) -> (match result with 
Some (a) -> (Some ( (a, acts)))
 | None  -> (None: ((state *  (operation) list)) option)))))) with 
Some (p) -> (match p with 
 (st0, acts) -> (Some ( (acts, st0))))
 | None  -> (None: (( (operation) list * state)) option)

let init (s : setupWchain) : state = 

let inner (cctx : cctx) (s : setupWchain) : (state) option = 
match init s.1 cctx s.0 with 
 (p, result) -> (match p with 
 (p0, y) -> (match p0 with 
 (c, c0) -> result)) in
let ctx = cctx_instance in
match (inner ctx s) with
  Some v -> v
| None -> (failwith (""): state)
type init_args_ty = setupWchain
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
      Some st -> (match (receive_wrapper dummy_chain cctx_instance  st p) with   
                    Some v -> (v.0, Some v.1)
                  | None -> (failwith ("") : return))
    | None -> (failwith ("cannot call this endpoint before Init has been called"): return))
let main (action, st : parameter_wrapper * state option) : return = wrapper (action, st)
