
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
let unsafe_int_to_nat (n : int) = abs(n)
  let predN (n : nat) = unsafe_int_to_nat (n - 1n)
  let mod_pow (a : int) (e : int) (p : int) : int = failwith ("unimplemented")
  let egcd (a : int) (p : int) : int * int = failwith ("unimplemented")

  let nth = let rec nth (n, l, default : nat * int list * int) : int =
  if n = 0n then (match l with
  [] -> default
   | x :: r -> x)
  else let m = predN n in (match l with
  [] -> default
   | x :: t -> (nth (m, t, default)))
   in fun (n:nat) (l:int list) (default:int) -> nth (n, l, default)


  let prod (l : int list) =
    List.fold (fun (a, b: int*int) -> multInt a b) l 1

  let firstn (n : nat) (l : int list) : int list =
    let (_,r) = List.fold_left (fun ((n, a),b : (nat * int list) * int) ->
        if n = 0n then (0n, a)
        else (predN n, b :: a)) (n,([] : int list)) l in
   r

  let skipn = let rec skipn (n, l : nat * int list) : int list =
   if n = 0n then l
    else let n0 = predN n in (match l with
    | [] -> ([]:int list)
    | a :: l0 -> (skipn (n0, l0 : nat * int list)))
    in fun (n : nat) (l : int list) -> skipn (n, l : nat * int list)
let hash_func (l : (nat) list) = addN 1n (List.fold_left (fun (a, p : nat * nat) -> Bitwise.xor p a) 1n l)

type setup = {
eligible_voters : (address, unit) map;
finish_registration_by : nat;
finish_commit_by : nat option;
finish_vote_by : nat;
registration_deposit : tez
}

type a = int

type voterInfo = {
voter_index : nat;
vote_hash : nat;
public_vote : a
}

type state = {
owner : address;
registered_voters : (address, voterInfo) map;
public_keys : a list;
setup : setup;
tally : nat option
}

type error = nat

type voteProof = ((((((((int * a) * a) * a) * a) * int) * int) * int) * int)

type msg = 
  Signup of (a * (a * int))
| Commit_to_vote of nat
| Submit_vote of (a * voteProof)
| Tally_votes


let default_error  : error = 
1n

let result_of_option(type t e) (o : t option) (err : e) : (t, e) result = 
match o with 
Some a0 -> ((Ok a0):(t, e) result)
 | None  -> ((Err err):(t, e) result)

let assert_false (check : bool) (chain : chain) (ctx : cctx) (state : state) (msg : msg option) (acts : operation list) : (((((chain * cctx) * state) * msg option) * operation list) * (unit, error) result) = 
(((((chain, ctx), state), msg), acts), (if check then ((Err default_error):(unit, error) result) else ((Ok ()):(unit, error) result)))

let assert_some(type a) (check : a option) (chain : chain) (ctx : cctx) (state : state) (msg : msg option) (acts : operation list) : (((((chain * cctx) * state) * msg option) * operation list) * (unit, error) result) = 
(((((chain, ctx), state), msg), acts), (match check with 
Some a -> ((Ok ()):(unit, error) result)
 | None  -> ((Err default_error):(unit, error) result)))

let assert_none(type a) (check : a option) (chain : chain) (ctx : cctx) (state : state) (msg : msg option) (acts : operation list) : (((((chain * cctx) * state) * msg option) * operation list) * (unit, error) result) = 
(((((chain, ctx), state), msg), acts), (match check with 
Some a -> ((Err default_error):(unit, error) result)
 | None  -> ((Ok ()):(unit, error) result)))

let assert_true (check : bool) (chain : chain) (ctx : cctx) (state : state) (msg : msg option) (acts : operation list) : (((((chain * cctx) * state) * msg option) * operation list) * (unit, error) result) = 
(((((chain, ctx), state), msg), acts), (if check then ((Ok ()):(unit, error) result) else ((Err default_error):(unit, error) result)))

let prime  : int = 
201697267445741585806196628073

let order  : int = 
prime

let generator  : int = 
3

let hash_sk_data (gv : a) (pk : a) (i : nat) : nat = 
hash_func ((unsafe_int_to_nat generator) :: ((unsafe_int_to_nat gv) :: ((unsafe_int_to_nat pk) :: (( i) :: ([]:nat list)))))

let elmeqb (a : a) (b : a) : bool = 
eqInt (modInt a prime) (modInt b prime)

let mul_p (a : int) (b : int) : int = 
modInt (multInt a b) prime

let mod_inv (a : int) (p : int) : int = 
let x = egcd a p in 
modInt x.0 p

let pow_p (a : int) (e : int) : int = 
mod_pow a e prime

let verify_secret_key_proof (pk : a) (i : nat) (proof : (a * int)) : bool = 
match proof with 
 (r0, gv0) -> (let z = int (hash_sk_data r0 pk i) in 
elmeqb r0 (mul_p (pow_p generator gv0) (pow_p pk z)))

let handle_signup (pk : a) (prf : (a * int)) (state : state) (caller : address) (cur_slot : nat) (chain : chain) (ctx : cctx) (state0 : state) (msg : msg option) (acts : operation list) : (((((chain * cctx) * state) * msg option) * operation list) * (state, error) result) = 
match assert_false (ltbN state.setup.finish_registration_by cur_slot) chain ctx state0 msg acts with 
 (res0, p0) -> (match res0 with 
 (acts0, p00) -> (match acts0 with 
 (msg0, p10) -> (match msg0 with 
 (state1, p20) -> (match state1 with 
 (ctx0, chain0) -> (match p0 with 
Ok res1 -> (match assert_some (Map.find_opt caller state.setup.eligible_voters) ctx0 chain0 p20 p10 p00 with 
 (res2, p3) -> (match res2 with 
 (acts1, p01) -> (match acts1 with 
 (msg1, p11) -> (match msg1 with 
 (state2, p21) -> (match state2 with 
 (ctx1, chain1) -> (match p3 with 
Ok res3 -> (match assert_none (Map.find_opt caller state.registered_voters) ctx1 chain1 p21 p11 p01 with 
 (res4, p4) -> (match res4 with 
 (acts2, p02) -> (match acts2 with 
 (msg2, p12) -> (match msg2 with 
 (state3, p22) -> (match state3 with 
 (ctx2, chain2) -> (match p4 with 
Ok res5 -> (match match ((ctx2, chain2), (ctx_amount chain2)) with 
 (res6, p5) -> (match res6 with 
 (ctx3, chain3) -> (((((ctx3, chain3), p22), p12), p02), ((Ok p5):(tez, error) result))) with 
 (res6, p5) -> (match res6 with 
 (acts3, p03) -> (match acts3 with 
 (msg3, p13) -> (match msg3 with 
 (state4, p23) -> (match state4 with 
 (ctx3, chain3) -> (match p5 with 
Ok res7 -> (match assert_true (eqTez res7 state.setup.registration_deposit) ctx3 chain3 p23 p13 p03 with 
 (res8, p6) -> (match res8 with 
 (acts4, p04) -> (match acts4 with 
 (msg4, p14) -> (match msg4 with 
 (state5, p24) -> (match state5 with 
 (ctx4, chain4) -> (match p6 with 
Ok res9 -> (match assert_true (ltInt (int (List.length state.public_keys)) (subInt order 2)) ctx4 chain4 p24 p14 p04 with 
 (res10, p7) -> (match res10 with 
 (acts5, p05) -> (match acts5 with 
 (msg5, p15) -> (match msg5 with 
 (state6, p25) -> (match state6 with 
 (ctx5, chain5) -> (match p7 with 
Ok res11 -> ((let index = List.length state.public_keys in 
fun (chain6 : chain) -> fun (ctx6 : cctx) -> fun (state7 : state) -> fun (msg6 : msg option) -> fun (acts6 : operation list) -> match assert_true (verify_secret_key_proof pk index prf) chain6 ctx6 state7 msg6 acts6 with 
 (res12, p8) -> (match res12 with 
 (acts7, p06) -> (match acts7 with 
 (msg7, p16) -> (match msg7 with 
 (state8, p26) -> (match state8 with 
 (ctx7, chain7) -> (match p8 with 
Ok res13 -> ((let inf = ({voter_index = index; vote_hash = 1n; public_vote = 0}: voterInfo) in 
let new_state = ({owner = state.owner; registered_voters = (Map.add caller inf state.registered_voters); public_keys = (List.fold_left (fun (acc, e : (int list) * int) -> e :: acc) state.public_keys (pk :: ([]:a list))); setup = state.setup; tally = state.tally}: state) in 
fun (chain8 : chain) -> fun (ctx8 : cctx) -> fun (state9 : state) -> fun (msg8 : msg option) -> fun (acts8 : operation list) -> (((((chain8, ctx8), state9), msg8), acts8), ((Ok new_state):(state, error) result))) ctx7 chain7 p26 p16 p06)
 | Err e0 -> (((((ctx7, chain7), p26), p16), p06), ((Err e0):(state, error) result)))))))) ctx5 chain5 p25 p15 p05)
 | Err e0 -> (((((ctx5, chain5), p25), p15), p05), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx4, chain4), p24), p14), p04), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx3, chain3), p23), p13), p03), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx2, chain2), p22), p12), p02), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx1, chain1), p21), p11), p01), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx0, chain0), p20), p10), p00), ((Err e0):(state, error) result)))))))

let voter_index (v : voterInfo) : nat = 
v.voter_index

let vote_hash (v : voterInfo) : nat = 
v.vote_hash

let public_vote (v : voterInfo) : a = 
v.public_vote

let set_VoterInfo_vote_hash (f : nat -> nat) (r : voterInfo) : voterInfo = 
({voter_index = (voter_index r); vote_hash = (f (vote_hash r)); public_vote = (public_vote r)}: voterInfo)

let owner (s : state) : address = 
s.owner

let registered_voters (s : state) : (address, voterInfo) map = 
s.registered_voters

let public_keys (s : state) : a list = 
s.public_keys

let setup (s : state) : setup = 
s.setup

let tally (s : state) : nat option = 
s.tally

let set_State_registered_voters (f : (address, voterInfo) map -> (address, voterInfo) map) (r : state) : state = 
({owner = (owner r); registered_voters = (f (registered_voters r)); public_keys = (public_keys r); setup = (setup r); tally = (tally r)}: state)

let handle_commit_to_vote (hash_ : nat) (state : state) (caller : address) (cur_slot : nat) (chain : chain) (ctx : cctx) (state0 : state) (msg : msg option) (acts : operation list) : (((((chain * cctx) * state) * msg option) * operation list) * (state, error) result) = 
match (((((chain, ctx), state0), msg), acts), (result_of_option state.setup.finish_commit_by default_error)) with 
 (res0, p0) -> (match res0 with 
 (acts0, p00) -> (match acts0 with 
 (msg0, p10) -> (match msg0 with 
 (state1, p20) -> (match state1 with 
 (ctx0, chain0) -> (match p0 with 
Ok res1 -> (match assert_false (ltbN res1 cur_slot) ctx0 chain0 p20 p10 p00 with 
 (res2, p3) -> (match res2 with 
 (acts1, p01) -> (match acts1 with 
 (msg1, p11) -> (match msg1 with 
 (state2, p21) -> (match state2 with 
 (ctx1, chain1) -> (match p3 with 
Ok res3 -> (match (((((ctx1, chain1), p21), p11), p01), (result_of_option (Map.find_opt caller state.registered_voters) default_error)) with 
 (res4, p4) -> (match res4 with 
 (acts2, p02) -> (match acts2 with 
 (msg2, p12) -> (match msg2 with 
 (state3, p22) -> (match state3 with 
 (ctx2, chain2) -> (match p4 with 
Ok res5 -> ((let inf = set_VoterInfo_vote_hash (fun (a : nat) -> hash_) res5 in 
fun (chain3 : chain) -> fun (ctx3 : cctx) -> fun (state4 : state) -> fun (msg3 : msg option) -> fun (acts3 : operation list) -> (((((chain3, ctx3), state4), msg3), acts3), ((Ok (set_State_registered_voters (Map.add caller inf) state)):(state, error) result))) ctx2 chain2 p22 p12 p02)
 | Err e0 -> (((((ctx2, chain2), p22), p12), p02), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx1, chain1), p21), p11), p01), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx0, chain0), p20), p10), p00), ((Err e0):(state, error) result)))))))

let hash_sv_data (i : nat) (pk : a) (rk : a) (a1 : a) (b1 : a) (a2 : a) (b2 : a) : nat = 
hash_func (( i) :: (List.map unsafe_int_to_nat (pk :: (rk :: (a1 :: (b1 :: (a2 :: (b2 :: ([]:a list)))))))))

let inv_p (a : int) : int = 
mod_inv a prime

let verify_secret_vote_proof (pk : a) (rk : a) (pv : a) (i : nat) (proof : voteProof) : bool = 
match proof with 
 (r20, p0) -> (match r20 with 
 (r10, p00) -> (match r10 with 
 (d20, p10) -> (match d20 with 
 (d10, p20) -> (match d10 with 
 (b20, p30) -> (match b20 with 
 (a20, p40) -> (match a20 with 
 (b10, p50) -> (match b10 with 
 (a10, w0) -> (let c = hash_sv_data i pk rk w0 p50 p40 p30 in 
andb (andb (andb (andb (eqInt (int c) (addInt p20 p10)) (elmeqb w0 (mul_p (pow_p generator p00) (pow_p pk p20)))) (elmeqb p50 (mul_p (pow_p rk p00) (pow_p pv p20)))) (elmeqb p40 (mul_p (pow_p generator p0) (pow_p pk p10)))) (elmeqb p30 (mul_p (pow_p rk p0) (pow_p (multInt pv (inv_p generator)) p10)))))))))))

let reconstructed_key (pks : a list) (n : nat) : a = 
let lprod = prod (firstn n pks) in 
let rprod = inv_p (prod (skipn (1n + n) pks)) in 
mul_p lprod rprod

let set_VoterInfo_public_vote (f : a -> a) (r : voterInfo) : voterInfo = 
({voter_index = (voter_index r); vote_hash = (vote_hash r); public_vote = (f (public_vote r))}: voterInfo)

let handle_submit_vote (v : a) (proof : voteProof) (state : state) (caller : address) (cur_slot : nat) (chain : chain) (ctx : cctx) (state0 : state) (msg : msg option) (acts : operation list) : (((((chain * cctx) * state) * msg option) * operation list) * (state, error) result) = 
match assert_false (ltbN state.setup.finish_vote_by cur_slot) chain ctx state0 msg acts with 
 (res0, p0) -> (match res0 with 
 (acts0, p00) -> (match acts0 with 
 (msg0, p10) -> (match msg0 with 
 (state1, p20) -> (match state1 with 
 (ctx0, chain0) -> (match p0 with 
Ok res1 -> (match (((((ctx0, chain0), p20), p10), p00), (result_of_option (Map.find_opt caller state.registered_voters) default_error)) with 
 (res2, p3) -> (match res2 with 
 (acts1, p01) -> (match acts1 with 
 (msg1, p11) -> (match msg1 with 
 (state2, p21) -> (match state2 with 
 (ctx1, chain1) -> (match p3 with 
Ok res3 -> (match (((((ctx1, chain1), p21), p11), p01), (match state.setup.finish_commit_by with 
Some a -> (if eqN (hash_func ((unsafe_int_to_nat v) :: ([]:nat list))) res3.vote_hash then ((Ok ()):(unit, error) result) else ((Err default_error):(unit, error) result))
 | None  -> ((Ok ()):(unit, error) result))) with 
 (res4, p4) -> (match res4 with 
 (acts2, p02) -> (match acts2 with 
 (msg2, p12) -> (match msg2 with 
 (state3, p22) -> (match state3 with 
 (ctx2, chain2) -> (match p4 with 
Ok res5 -> (match (((((ctx2, chain2), p22), p12), p02), (if verify_secret_vote_proof (nth res3.voter_index state.public_keys 0) (reconstructed_key state.public_keys res3.voter_index) v res3.voter_index proof then ((Ok ()):(unit, error) result) else ((Err default_error):(unit, error) result))) with 
 (res6, p5) -> (match res6 with 
 (acts3, p03) -> (match acts3 with 
 (msg3, p13) -> (match msg3 with 
 (state4, p23) -> (match state4 with 
 (ctx3, chain3) -> (match p5 with 
Ok res7 -> ((let inf = set_VoterInfo_public_vote (fun (a : a) -> v) res3 in 
fun (chain4 : chain) -> fun (ctx4 : cctx) -> fun (state5 : state) -> fun (msg4 : msg option) -> fun (acts4 : operation list) -> (((((chain4, ctx4), state5), msg4), acts4), ((Ok (set_State_registered_voters (Map.add caller inf) state)):(state, error) result))) ctx3 chain3 p23 p13 p03)
 | Err e0 -> (((((ctx3, chain3), p23), p13), p03), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx2, chain2), p22), p12), p02), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx1, chain1), p21), p11), p01), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx0, chain0), p20), p10), p00), ((Err e0):(state, error) result)))))))

let set_State_tally (f : nat option -> nat option) (r : state) : state = 
({owner = (owner r); registered_voters = (registered_voters r); public_keys = (public_keys r); setup = (setup r); tally = (f (tally r))}: state)

let handle_tally_votes (state : state) (cur_slot : nat) (chain : chain) (ctx : cctx) (state0 : state) (msg : msg option) (acts : operation list) : (((((chain * cctx) * state) * msg option) * operation list) * (state, error) result) = 
match assert_false (ltbN cur_slot state.setup.finish_vote_by) chain ctx state0 msg acts with 
 (res0, p0) -> (match res0 with 
 (acts0, p00) -> (match acts0 with 
 (msg0, p10) -> (match msg0 with 
 (state1, p20) -> (match state1 with 
 (ctx0, chain0) -> (match p0 with 
Ok res1 -> (match assert_none state.tally ctx0 chain0 p20 p10 p00 with 
 (res2, p3) -> (match res2 with 
 (acts1, p01) -> (match acts1 with 
 (msg1, p11) -> (match msg1 with 
 (state2, p21) -> (match state2 with 
 (ctx1, chain1) -> (match p3 with 
Ok res3 -> ((let voters = (fun (v:(address, voterInfo) map) ->
    Map.fold (fun (acc, (_,info) : voterInfo list * (address * voterInfo)) -> info :: acc)
    v ([]: voterInfo list)) state.registered_voters in 
fun (chain2 : chain) -> fun (ctx2 : cctx) -> fun (state3 : state) -> fun (msg2 : msg option) -> fun (acts2 : operation list) -> match assert_false ((let existsb (f : voterInfo -> bool) = let rec existsb (l: voterInfo list) : bool =
  match l with
  [] -> false
  | a :: l0 -> (if (f a) then true else (existsb (l0)))
  in fun (l: voterInfo list) -> existsb (l) in existsb) (fun (vi : voterInfo) -> if elmeqb vi.public_vote 0 then true else false) voters) chain2 ctx2 state3 msg2 acts2 with 
 (res4, p4) -> (match res4 with 
 (acts3, p02) -> (match acts3 with 
 (msg3, p12) -> (match msg3 with 
 (state4, p22) -> (match state4 with 
 (ctx3, chain3) -> (match p4 with 
Ok res5 -> ((let votes = List.map public_vote voters in 
fun (chain4 : chain) -> fun (ctx4 : cctx) -> fun (state5 : state) -> fun (msg4 : msg option) -> fun (acts4 : operation list) -> match (((((chain4, ctx4), state5), msg4), acts4), ((fun (votes : (a) list) ->
  let rec bruteforce_tally_aux (n, votes_product : nat * a) : (nat, nat) result =
    if elmeqb (pow_p generator (int n)) votes_product then
        Ok (n)
    else if n = 0n then
      Err 0n
    else
      let n0 = n - 1n in
        (bruteforce_tally_aux (unsafe_int_to_nat n0, votes_product))
  in bruteforce_tally_aux ((List.length votes), (prod votes))) votes)) with 
 (res6, p5) -> (match res6 with 
 (acts5, p03) -> (match acts5 with 
 (msg5, p13) -> (match msg5 with 
 (state6, p23) -> (match state6 with 
 (ctx5, chain5) -> (match p5 with 
Ok res7 -> (((((ctx5, chain5), p23), p13), p03), ((Ok (set_State_tally (fun (a : nat option) -> Some res7) state)):(state, error) result))
 | Err e0 -> (((((ctx5, chain5), p23), p13), p03), ((Err e0):(state, error) result)))))))) ctx3 chain3 p22 p12 p02)
 | Err e0 -> (((((ctx3, chain3), p22), p12), p02), ((Err e0):(state, error) result)))))))) ctx1 chain1 p21 p11 p01)
 | Err e0 -> (((((ctx1, chain1), p21), p11), p01), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx0, chain0), p20), p10), p00), ((Err e0):(state, error) result)))))))

let receive (chain : chain) (ctx : cctx) (state : state) (msg : msg option) (acts : operation list) : (((((chain * cctx) * state) * msg option) * operation list) * (state, error) result) = 
match (((((chain, ctx), state), msg), acts), ((Ok state):(state, error) result)) with 
 (res0, p0) -> (match res0 with 
 (acts0, p00) -> (match acts0 with 
 (msg0, p10) -> (match msg0 with 
 (state0, p20) -> (match state0 with 
 (ctx0, chain0) -> (match p0 with 
Ok res1 -> (match match ((ctx0, chain0), (ctx_from chain0)) with 
 (res2, p3) -> (match res2 with 
 (ctx1, chain1) -> (((((ctx1, chain1), p20), p10), p00), ((Ok p3):(address, error) result))) with 
 (res2, p3) -> (match res2 with 
 (acts1, p01) -> (match acts1 with 
 (msg1, p11) -> (match msg1 with 
 (state1, p21) -> (match state1 with 
 (ctx1, chain1) -> (match p3 with 
Ok res3 -> (match match ((ctx1, chain1), (current_slot ctx1)) with 
 (res4, p4) -> (match res4 with 
 (ctx2, chain2) -> (((((ctx2, chain2), p21), p11), p01), ((Ok p4):(nat, error) result))) with 
 (res4, p4) -> (match res4 with 
 (acts2, p02) -> (match acts2 with 
 (msg2, p12) -> (match msg2 with 
 (state2, p22) -> (match state2 with 
 (ctx2, chain2) -> (match p4 with 
Ok res5 -> (match (((((ctx2, chain2), p22), p12), p02), (result_of_option p12 default_error)) with 
 (res6, p5) -> (match res6 with 
 (acts3, p03) -> (match acts3 with 
 (msg3, p13) -> (match msg3 with 
 (state3, p23) -> (match state3 with 
 (ctx3, chain3) -> (match p5 with 
Ok res7 -> ((match res7 with 
Signup (prf0, pk0) -> (handle_signup prf0 pk0 res1 res3 res5)
 | Commit_to_vote hash_0 -> (handle_commit_to_vote hash_0 res1 res3 res5)
 | Submit_vote (proof0, v0) -> (handle_submit_vote proof0 v0 res1 res3 res5)
 | Tally_votes  -> (handle_tally_votes res1 res5)) ctx3 chain3 p23 p13 p03)
 | Err e0 -> (((((ctx3, chain3), p23), p13), p03), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx2, chain2), p22), p12), p02), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx1, chain1), p21), p11), p01), ((Err e0):(state, error) result))))))))
 | Err e0 -> (((((ctx0, chain0), p20), p10), p00), ((Err e0):(state, error) result)))))))

let receive_wrapper (c : chain) (ctx : cctx) (st : state) (msg : msg option) : ((operation list * state), error) result = 
match match receive c ctx st msg ([]:operation list) with 
 (result0, p0) -> (match result0 with 
 (acts0, p00) -> (match acts0 with 
 (o0, p10) -> (match o0 with 
 (y0, p20) -> (match y0 with 
 (c00, c1) -> (match p0 with 
Ok res0 -> ((Ok (res0, p00)):((state * operation list), error) result)
 | Err e0 -> ((Err e0):((state * operation list), error) result)))))) with 
Ok t0 -> (match t0 with 
 (acts0, st0) -> ((Ok (st0, acts0)):((operation list * state), error) result))
 | Err e0 -> ((Err e0):((operation list * state), error) result)

let init (s : (address * setup)) : (state, error) result = let inner (s : (address * setup)) :(state, error) result = 
if ltbN s.1.finish_registration_by s.1.finish_vote_by then ((Ok ({owner = s.0; registered_voters = (Map.empty: (address, voterInfo) map); public_keys = ([]:a list); setup = s.1; tally = (None:nat option)}: state)):(state, error) result) else ((Err default_error):(state, error) result) in
match (inner s) with
  Ok v -> Ok v
| Err e -> (failwith e: (state, error) result)


type return = (operation) list * state

let main (p, st : msg option * state) : return = 
   (match (receive_wrapper dummy_chain cctx_instance  st p) with 
      Ok v -> (v.0, v.1)
    | Err e -> (failwith e : return))
