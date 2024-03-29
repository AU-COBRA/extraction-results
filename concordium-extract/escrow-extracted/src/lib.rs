#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use concordium_std::*;
use concert_std::{ActionBody, ConCertDeserial, ConCertSerial, SerializedValue};
use core::marker::PhantomData;
use immutable_map::TreeMap;
use std::convert::TryFrom;

fn __nat_succ(x: u64) -> u64 {
  x.checked_add(1).unwrap()
}

macro_rules! __nat_elim {
    ($zcase:expr, $pred:ident, $scase:expr, $val:expr) => {
        { let v = $val;
        if v == 0 { $zcase } else { let $pred = v - 1; $scase } }
    }
}

macro_rules! __andb { ($b1:expr, $b2:expr) => { $b1 && $b2 } }
macro_rules! __orb { ($b1:expr, $b2:expr) => { $b1 || $b2 } }

fn __pos_onebit(x: u64) -> u64 {
  x.checked_mul(2).unwrap() + 1
}

fn __pos_zerobit(x: u64) -> u64 {
  x.checked_mul(2).unwrap()
}

macro_rules! __pos_elim {
    ($p:ident, $onebcase:expr, $p2:ident, $zerobcase:expr, $onecase:expr, $val:expr) => {
        {
            let n = $val;
            if n == 1 {
                $onecase
            } else if (n & 1) == 0 {
                let $p2 = n >> 1;
                $zerobcase
            } else {
                let $p = n >> 1;
                $onebcase
            }
        }
    }
}

fn __Z_frompos(z: u64) -> i64 {
  use std::convert::TryFrom;

  i64::try_from(z).unwrap()
}

fn __Z_fromneg(z : u64) -> i64 {
  use std::convert::TryFrom;

  i64::try_from(z).unwrap().checked_neg().unwrap()
}

macro_rules! __Z_elim {
    ($zero_case:expr, $p:ident, $pos_case:expr, $p2:ident, $neg_case:expr, $val:expr) => {
        {
            let n = $val;
            if n == 0 {
                $zero_case
            } else if n < 0 {
                let $p2 = n.unsigned_abs();
                $neg_case
            } else {
                let $p = n as u64;
                $pos_case
            }
        }
    }
}

fn __N_frompos(z: u64) -> u64 {
  z
}

macro_rules! __N_elim {
    ($zero_case:expr, $p:ident, $pos_case:expr, $val:expr) => {
        { let $p = $val; if $p == 0 { $zero_case } else { $pos_case } }
    }
}

type __pair<A, B> = (A, B);

macro_rules! __pair_elim {
    ($fst:ident, $snd:ident, $body:expr, $p:expr) => {
        { let ($fst, $snd) = $p; $body }
    }
}

fn __mk_pair<A: Copy, B: Copy>(a: A, b: B) -> __pair<A, B> { (a, b) }

fn hint_app<TArg, TRet>(f: &dyn Fn(TArg) -> TRet) -> &dyn Fn(TArg) -> TRet {
  f
}

#[derive(Debug, PartialEq, Eq)]
enum InitError {
   DeserialParams,
   SerialParams,
   Error(u64)
}

impl From<InitError> for Reject {
    fn from(err: InitError) -> Self {
        let error_code = match err {
            InitError::DeserialParams => unsafe {
                crate::num::NonZeroI32::new_unchecked(-42000001)
            },
            InitError::SerialParams => unsafe {
                crate::num::NonZeroI32::new_unchecked(-42000002)
            },
            InitError::Error(error) => unsafe {
              match i32::try_from(error).ok() {
                Option::Some(0) => {
                  crate::num::NonZeroI32::new_unchecked(i32::MIN)
                },
                Option::Some(v) => {
                  crate::num::NonZeroI32::new_unchecked(v)
                },
                Option::None => {
                  crate::num::NonZeroI32::new_unchecked(i32::MIN)
                }
              }
            }
        };
        Self {
            error_code
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ReceiveError {
    DeserialMsg,
    DeserialOldState,
    SerialNewState,
    ConvertActions, // Cannot convert ConCert actions to Concordium actions
    Error(u64)
}

impl From<ReceiveError> for Reject {
    fn from(err: ReceiveError) -> Self {
        let error_code = match err {
            ReceiveError::DeserialMsg => unsafe {
                crate::num::NonZeroI32::new_unchecked(-42000001)
            },
            ReceiveError::DeserialOldState => unsafe {
                crate::num::NonZeroI32::new_unchecked(-42000002)
            },
            ReceiveError::SerialNewState => unsafe {
                crate::num::NonZeroI32::new_unchecked(-42000003)
            },
            ReceiveError::ConvertActions => unsafe {
                crate::num::NonZeroI32::new_unchecked(-42000004)
            },
            ReceiveError::Error(error) => unsafe {
              match i32::try_from(error).ok() {
                Option::Some(0) => {
                  crate::num::NonZeroI32::new_unchecked(i32::MIN)
                },
                Option::Some(v) => {
                  crate::num::NonZeroI32::new_unchecked(v)
                },
                Option::None => {
                  crate::num::NonZeroI32::new_unchecked(i32::MIN)
                }
              }
            }
        };
        Self {
            error_code
        }
    }
}

#[derive(Clone, ConCertSerial, ConCertDeserial, PartialEq)]
pub enum Coq_Init_Datatypes_list<'a, A> {
  nil(PhantomData<&'a A>),
  cons(PhantomData<&'a A>, A, &'a Coq_Init_Datatypes_list<'a, A>)
}

#[derive(Clone, ConCertSerial, ConCertDeserial, PartialEq)]
pub enum ConCert_Execution_Blockchain_Chain<'a> {
  build_chain(PhantomData<&'a ()>, u64, u64, u64)
}

type ConCert_Execution_Blockchain_Address<'a> = concordium_std::Address;

type ConCert_Execution_Blockchain_Amount<'a> = i64;

#[derive(Clone, ConCertSerial, ConCertDeserial, PartialEq)]
pub enum ConCert_Execution_Blockchain_ContractCallContext<'a> {
  build_ctx(PhantomData<&'a ()>, ConCert_Execution_Blockchain_Address<'a>, ConCert_Execution_Blockchain_Address<'a>, ConCert_Execution_Blockchain_Address<'a>, ConCert_Execution_Blockchain_Amount<'a>, ConCert_Execution_Blockchain_Amount<'a>)
}

#[derive(Clone, ConCertSerial, ConCertDeserial, PartialEq)]
pub enum ConCert_Examples_Escrow_Escrow_Setup<'a> {
  build_setup(PhantomData<&'a ()>, ConCert_Execution_Blockchain_Address<'a>)
}

#[derive(Clone, ConCertSerial, ConCertDeserial, PartialEq)]
pub enum ConCert_Examples_Escrow_Escrow_NextStep<'a> {
  buyer_commit(PhantomData<&'a ()>),
  buyer_confirm(PhantomData<&'a ()>),
  withdrawals(PhantomData<&'a ()>),
  no_next_step(PhantomData<&'a ()>)
}

#[derive(Clone, ConCertSerial, ConCertDeserial, PartialEq)]
pub enum ConCert_Examples_Escrow_Escrow_State<'a> {
  build_state(PhantomData<&'a ()>, u64, &'a ConCert_Examples_Escrow_Escrow_NextStep<'a>, ConCert_Execution_Blockchain_Address<'a>, ConCert_Execution_Blockchain_Address<'a>, ConCert_Execution_Blockchain_Amount<'a>, ConCert_Execution_Blockchain_Amount<'a>)
}

type ConCert_Examples_Escrow_Escrow_Error<'a> = u64;

#[derive(Clone, ConCertSerial, ConCertDeserial, PartialEq)]
pub enum ConCert_Examples_Escrow_Escrow_Msg<'a> {
  commit_money(PhantomData<&'a ()>),
  confirm_item_received(PhantomData<&'a ()>),
  withdraw(PhantomData<&'a ()>)
}

struct Program {
  __alloc: bumpalo::Bump,
}

impl<'a> Program {
fn new() -> Self {
  Program {
    __alloc: bumpalo::Bump::new(),
  }
}

fn alloc<T>(&'a self, t: T) -> &'a T {
  self.__alloc.alloc(t)
}

fn closure<TArg, TRet>(&'a self, F: impl Fn(TArg) -> TRet + 'a) -> &'a dyn Fn(TArg) -> TRet {
  self.__alloc.alloc(F)
}

fn ConCert_Execution_Blockchain_ctx_from(&'a self, c: &'a ConCert_Execution_Blockchain_ContractCallContext<'a>) -> ConCert_Execution_Blockchain_Address<'a> {
  match c {
    &ConCert_Execution_Blockchain_ContractCallContext::build_ctx(_, ctx_origin, ctx_from, ctx_contract_address, ctx_contract_balance, ctx_amount) => {
      ctx_from
    },
  }
}
fn ConCert_Execution_Blockchain_ctx_from__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Execution_Blockchain_ContractCallContext<'a>) -> ConCert_Execution_Blockchain_Address<'a> {
  self.closure(move |c| {
    self.ConCert_Execution_Blockchain_ctx_from(
      c)
  })
}

fn ConCert_Examples_Escrow_Escrow_setup_buyer(&'a self, s: &'a ConCert_Examples_Escrow_Escrow_Setup<'a>) -> ConCert_Execution_Blockchain_Address<'a> {
  match s {
    &ConCert_Examples_Escrow_Escrow_Setup::build_setup(_, setup_buyer) => {
      setup_buyer
    },
  }
}
fn ConCert_Examples_Escrow_Escrow_setup_buyer__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_Setup<'a>) -> ConCert_Execution_Blockchain_Address<'a> {
  self.closure(move |s| {
    self.ConCert_Examples_Escrow_Escrow_setup_buyer(
      s)
  })
}

fn ConCert_Execution_Blockchain_address_eqb(&'a self) -> impl Fn(concordium_std::Address) -> &'a dyn Fn(concordium_std::Address) -> bool { move |a| self.alloc(move |b| a == b) }

fn ConCert_Examples_Escrow_Escrow_default_error(&'a self) -> ConCert_Examples_Escrow_Escrow_Error<'a> {
  __nat_succ(
    0)
}

fn Coq_ZArith_BinIntDef_Z_eqb(&'a self, a: i64, b: i64) -> bool { a == b }
fn Coq_ZArith_BinIntDef_Z_eqb__curried(&'a self) -> &'a dyn Fn(i64) -> &'a dyn Fn(i64) -> bool {
  self.closure(move |x| {
    self.closure(move |y| {
      self.Coq_ZArith_BinIntDef_Z_eqb(
        x,
        y)
    })
  })
}

fn ConCert_Execution_Blockchain_ctx_amount(&'a self, c: &'a ConCert_Execution_Blockchain_ContractCallContext<'a>) -> ConCert_Execution_Blockchain_Amount<'a> {
  match c {
    &ConCert_Execution_Blockchain_ContractCallContext::build_ctx(_, ctx_origin, ctx_from, ctx_contract_address, ctx_contract_balance, ctx_amount) => {
      ctx_amount
    },
  }
}
fn ConCert_Execution_Blockchain_ctx_amount__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Execution_Blockchain_ContractCallContext<'a>) -> ConCert_Execution_Blockchain_Amount<'a> {
  self.closure(move |c| {
    self.ConCert_Execution_Blockchain_ctx_amount(
      c)
  })
}

fn Coq_ZArith_BinIntDef_Z_even(&'a self, a: i64) -> bool { a.checked_rem(2).unwrap() == 0 }
fn Coq_ZArith_BinIntDef_Z_even__curried(&'a self) -> &'a dyn Fn(i64) -> bool {
  self.closure(move |z| {
    self.Coq_ZArith_BinIntDef_Z_even(
      z)
  })
}

fn ConCert_Execution_Blockchain_current_slot(&'a self, c: &'a ConCert_Execution_Blockchain_Chain<'a>) -> u64 {
  match c {
    &ConCert_Execution_Blockchain_Chain::build_chain(_, chain_height, current_slot, finalized_height) => {
      current_slot
    },
  }
}
fn ConCert_Execution_Blockchain_current_slot__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Execution_Blockchain_Chain<'a>) -> u64 {
  self.closure(move |c| {
    self.ConCert_Execution_Blockchain_current_slot(
      c)
  })
}

fn ConCert_Examples_Escrow_Escrow_init(&'a self, chain: &'a ConCert_Execution_Blockchain_Chain<'a>, ctx: &'a ConCert_Execution_Blockchain_ContractCallContext<'a>, setup: &'a ConCert_Examples_Escrow_Escrow_Setup<'a>) -> Result<&'a ConCert_Examples_Escrow_Escrow_State<'a>, ConCert_Examples_Escrow_Escrow_Error<'a>> {
  let seller =
    self.ConCert_Execution_Blockchain_ctx_from(
      ctx);
  let buyer =
    self.ConCert_Examples_Escrow_Escrow_setup_buyer(
      setup);
  match match self.ConCert_Execution_Blockchain_address_eqb()(
                buyer)(
                seller) {
          true => {
            Err(
              self.ConCert_Examples_Escrow_Escrow_default_error())
          },
          false => {
            Ok(
              ())
          },
        } {
    Ok(t) => {
      match match self.Coq_ZArith_BinIntDef_Z_eqb(
                    self.ConCert_Execution_Blockchain_ctx_amount(
                      ctx),
                    0) {
              true => {
                Err(
                  self.ConCert_Examples_Escrow_Escrow_default_error())
              },
              false => {
                Ok(
                  ())
              },
            } {
        Ok(t2) => {
          match match self.Coq_ZArith_BinIntDef_Z_even(
                        self.ConCert_Execution_Blockchain_ctx_amount(
                          ctx)) {
                  true => {
                    Ok(
                      ())
                  },
                  false => {
                    Err(
                      self.ConCert_Examples_Escrow_Escrow_default_error())
                  },
                } {
            Ok(t3) => {
              Ok(
                self.alloc(
                  ConCert_Examples_Escrow_Escrow_State::build_state(
                    PhantomData,
                    self.ConCert_Execution_Blockchain_current_slot(
                      chain),
                    self.alloc(
                      ConCert_Examples_Escrow_Escrow_NextStep::buyer_commit(
                        PhantomData)),
                    seller,
                    buyer,
                    0,
                    0)))
            },
            Err(e) => {
              Err(
                e)
            },
          }
        },
        Err(e) => {
          Err(
            e)
        },
      }
    },
    Err(e) => {
      Err(
        e)
    },
  }
}
fn ConCert_Examples_Escrow_Escrow_init__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Execution_Blockchain_Chain<'a>) -> &'a dyn Fn(&'a ConCert_Execution_Blockchain_ContractCallContext<'a>) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_Setup<'a>) -> Result<&'a ConCert_Examples_Escrow_Escrow_State<'a>, ConCert_Examples_Escrow_Escrow_Error<'a>> {
  self.closure(move |chain| {
    self.closure(move |ctx| {
      self.closure(move |setup| {
        self.ConCert_Examples_Escrow_Escrow_init(
          chain,
          ctx,
          setup)
      })
    })
  })
}

fn ConCert_Examples_Escrow_Escrow_next_step(&'a self, s: &'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a ConCert_Examples_Escrow_Escrow_NextStep<'a> {
  match s {
    &ConCert_Examples_Escrow_Escrow_State::build_state(_, last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) => {
      next_step
    },
  }
}
fn ConCert_Examples_Escrow_Escrow_next_step__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a ConCert_Examples_Escrow_Escrow_NextStep<'a> {
  self.closure(move |s| {
    self.ConCert_Examples_Escrow_Escrow_next_step(
      s)
  })
}

fn ConCert_Execution_ResultMonad_result_of_option<T: Copy, E: Copy>(&'a self, o: Option<T>, err: E) -> Result<T, E> {
  match o {
    Some(a) => {
      Ok(
        a)
    },
    None => {
      Err(
        err)
    },
  }
}
fn ConCert_Execution_ResultMonad_result_of_option__curried<T: Copy, E: Copy>(&'a self) -> &'a dyn Fn(Option<T>) -> &'a dyn Fn(E) -> Result<T, E> {
  self.closure(move |o| {
    self.closure(move |err| {
      self.ConCert_Execution_ResultMonad_result_of_option(
        o,
        err)
    })
  })
}

fn Coq_ZArith_BinIntDef_Z_ltb(&'a self, a: i64, b: i64) -> bool { a < b }
fn Coq_ZArith_BinIntDef_Z_ltb__curried(&'a self) -> &'a dyn Fn(i64) -> &'a dyn Fn(i64) -> bool {
  self.closure(move |x| {
    self.closure(move |y| {
      self.Coq_ZArith_BinIntDef_Z_ltb(
        x,
        y)
    })
  })
}

fn Coq_ZArith_BinIntDef_Z_add(&'a self, a: i64, b: i64) -> i64 { a.checked_add(b).unwrap() }
fn Coq_ZArith_BinIntDef_Z_add__curried(&'a self) -> &'a dyn Fn(i64) -> &'a dyn Fn(i64) -> i64 {
  self.closure(move |x| {
    self.closure(move |y| {
      self.Coq_ZArith_BinIntDef_Z_add(
        x,
        y)
    })
  })
}

fn Coq_ZArith_BinIntDef_Z_opp(&'a self, a: i64) -> i64 { a.checked_neg().unwrap() }
fn Coq_ZArith_BinIntDef_Z_opp__curried(&'a self) -> &'a dyn Fn(i64) -> i64 {
  self.closure(move |x| {
    self.Coq_ZArith_BinIntDef_Z_opp(
      x)
  })
}

fn Coq_ZArith_BinIntDef_Z_sub(&'a self, a: i64, b: i64) -> i64 { a.checked_sub(b).unwrap() }
fn Coq_ZArith_BinIntDef_Z_sub__curried(&'a self) -> &'a dyn Fn(i64) -> &'a dyn Fn(i64) -> i64 {
  self.closure(move |m| {
    self.closure(move |n| {
      self.Coq_ZArith_BinIntDef_Z_sub(
        m,
        n)
    })
  })
}

fn ConCert_Examples_Escrow_Escrow_subAmountOption(&'a self, n: ConCert_Execution_Blockchain_Amount<'a>, m: ConCert_Execution_Blockchain_Amount<'a>) -> Option<ConCert_Execution_Blockchain_Amount<'a>> {
  match self.Coq_ZArith_BinIntDef_Z_ltb(
          n,
          m) {
    true => {
      None
    },
    false => {
      Some(
        self.Coq_ZArith_BinIntDef_Z_sub(
          n,
          m))
    },
  }
}
fn ConCert_Examples_Escrow_Escrow_subAmountOption__curried(&'a self) -> &'a dyn Fn(ConCert_Execution_Blockchain_Amount<'a>) -> &'a dyn Fn(ConCert_Execution_Blockchain_Amount<'a>) -> Option<ConCert_Execution_Blockchain_Amount<'a>> {
  self.closure(move |n| {
    self.closure(move |m| {
      self.ConCert_Examples_Escrow_Escrow_subAmountOption(
        n,
        m)
    })
  })
}

fn ConCert_Execution_Blockchain_ctx_contract_balance(&'a self, c: &'a ConCert_Execution_Blockchain_ContractCallContext<'a>) -> ConCert_Execution_Blockchain_Amount<'a> {
  match c {
    &ConCert_Execution_Blockchain_ContractCallContext::build_ctx(_, ctx_origin, ctx_from, ctx_contract_address, ctx_contract_balance, ctx_amount) => {
      ctx_contract_balance
    },
  }
}
fn ConCert_Execution_Blockchain_ctx_contract_balance__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Execution_Blockchain_ContractCallContext<'a>) -> ConCert_Execution_Blockchain_Amount<'a> {
  self.closure(move |c| {
    self.ConCert_Execution_Blockchain_ctx_contract_balance(
      c)
  })
}

fn Coq_ZArith_BinIntDef_Z_mul(&'a self, a: i64, b: i64) -> i64 { a.checked_mul(b).unwrap() }
fn Coq_ZArith_BinIntDef_Z_mul__curried(&'a self) -> &'a dyn Fn(i64) -> &'a dyn Fn(i64) -> i64 {
  self.closure(move |x| {
    self.closure(move |y| {
      self.Coq_ZArith_BinIntDef_Z_mul(
        x,
        y)
    })
  })
}

fn Coq_ZArith_BinIntDef_Z_leb(&'a self, a: i64, b: i64) -> bool { a <= b }
fn Coq_ZArith_BinIntDef_Z_leb__curried(&'a self) -> &'a dyn Fn(i64) -> &'a dyn Fn(i64) -> bool {
  self.closure(move |x| {
    self.closure(move |y| {
      self.Coq_ZArith_BinIntDef_Z_leb(
        x,
        y)
    })
  })
}

fn Coq_ZArith_BinIntDef_Z_pos_div_eucl(&'a self, a: u64, b: i64) -> __pair<i64, i64> {
  __pos_elim!(
    a2, {
      __pair_elim!(
        r, q, {
          let r2 =
            self.Coq_ZArith_BinIntDef_Z_add(
              self.Coq_ZArith_BinIntDef_Z_mul(
                __Z_frompos(
                  __pos_zerobit(
                    1)),
                q),
              __Z_frompos(
                1));
          match self.Coq_ZArith_BinIntDef_Z_ltb(
                  r2,
                  b) {
            true => {
              __mk_pair(
                self.Coq_ZArith_BinIntDef_Z_mul(
                  __Z_frompos(
                    __pos_zerobit(
                      1)),
                  r),
                r2)
            },
            false => {
              __mk_pair(
                self.Coq_ZArith_BinIntDef_Z_add(
                  self.Coq_ZArith_BinIntDef_Z_mul(
                    __Z_frompos(
                      __pos_zerobit(
                        1)),
                    r),
                  __Z_frompos(
                    1)),
                self.Coq_ZArith_BinIntDef_Z_sub(
                  r2,
                  b))
            },
          }
        },
        self.Coq_ZArith_BinIntDef_Z_pos_div_eucl(
          a2,
          b))
    },
    a2, {
      __pair_elim!(
        r, q, {
          let r2 =
            self.Coq_ZArith_BinIntDef_Z_mul(
              __Z_frompos(
                __pos_zerobit(
                  1)),
              q);
          match self.Coq_ZArith_BinIntDef_Z_ltb(
                  r2,
                  b) {
            true => {
              __mk_pair(
                self.Coq_ZArith_BinIntDef_Z_mul(
                  __Z_frompos(
                    __pos_zerobit(
                      1)),
                  r),
                r2)
            },
            false => {
              __mk_pair(
                self.Coq_ZArith_BinIntDef_Z_add(
                  self.Coq_ZArith_BinIntDef_Z_mul(
                    __Z_frompos(
                      __pos_zerobit(
                        1)),
                    r),
                  __Z_frompos(
                    1)),
                self.Coq_ZArith_BinIntDef_Z_sub(
                  r2,
                  b))
            },
          }
        },
        self.Coq_ZArith_BinIntDef_Z_pos_div_eucl(
          a2,
          b))
    },
    {
      match self.Coq_ZArith_BinIntDef_Z_leb(
              __Z_frompos(
                __pos_zerobit(
                  1)),
              b) {
        true => {
          __mk_pair(
            0,
            __Z_frompos(
              1))
        },
        false => {
          __mk_pair(
            __Z_frompos(
              1),
            0)
        },
      }
    },
    a)
}
fn Coq_ZArith_BinIntDef_Z_pos_div_eucl__curried(&'a self) -> &'a dyn Fn(u64) -> &'a dyn Fn(i64) -> __pair<i64, i64> {
  self.closure(move |a| {
    self.closure(move |b| {
      self.Coq_ZArith_BinIntDef_Z_pos_div_eucl(
        a,
        b)
    })
  })
}

fn Coq_ZArith_BinIntDef_Z_div_eucl(&'a self, a: i64, b: i64) -> __pair<i64, i64> {
  __Z_elim!(
    {
      __mk_pair(
        0,
        0)
    },
    a2, {
      __Z_elim!(
        {
          __mk_pair(
            0,
            a)
        },
        p, {
          self.Coq_ZArith_BinIntDef_Z_pos_div_eucl(
            a2,
            b)
        },
        b2, {
          __pair_elim!(
            r, q, {
              __Z_elim!(
                {
                  __mk_pair(
                    self.Coq_ZArith_BinIntDef_Z_opp(
                      r),
                    0)
                },
                p, {
                  __mk_pair(
                    self.Coq_ZArith_BinIntDef_Z_opp(
                      self.Coq_ZArith_BinIntDef_Z_add(
                        r,
                        __Z_frompos(
                          1))),
                    self.Coq_ZArith_BinIntDef_Z_add(
                      b,
                      q))
                },
                p, {
                  __mk_pair(
                    self.Coq_ZArith_BinIntDef_Z_opp(
                      self.Coq_ZArith_BinIntDef_Z_add(
                        r,
                        __Z_frompos(
                          1))),
                    self.Coq_ZArith_BinIntDef_Z_add(
                      b,
                      q))
                },
                q)
            },
            self.Coq_ZArith_BinIntDef_Z_pos_div_eucl(
              a2,
              __Z_frompos(
                b2)))
        },
        b)
    },
    a2, {
      __Z_elim!(
        {
          __mk_pair(
            0,
            a)
        },
        p, {
          __pair_elim!(
            r, q, {
              __Z_elim!(
                {
                  __mk_pair(
                    self.Coq_ZArith_BinIntDef_Z_opp(
                      r),
                    0)
                },
                p2, {
                  __mk_pair(
                    self.Coq_ZArith_BinIntDef_Z_opp(
                      self.Coq_ZArith_BinIntDef_Z_add(
                        r,
                        __Z_frompos(
                          1))),
                    self.Coq_ZArith_BinIntDef_Z_sub(
                      b,
                      q))
                },
                p2, {
                  __mk_pair(
                    self.Coq_ZArith_BinIntDef_Z_opp(
                      self.Coq_ZArith_BinIntDef_Z_add(
                        r,
                        __Z_frompos(
                          1))),
                    self.Coq_ZArith_BinIntDef_Z_sub(
                      b,
                      q))
                },
                q)
            },
            self.Coq_ZArith_BinIntDef_Z_pos_div_eucl(
              a2,
              b))
        },
        b2, {
          __pair_elim!(
            r, q, {
              __mk_pair(
                r,
                self.Coq_ZArith_BinIntDef_Z_opp(
                  q))
            },
            self.Coq_ZArith_BinIntDef_Z_pos_div_eucl(
              a2,
              __Z_frompos(
                b2)))
        },
        b)
    },
    a)
}
fn Coq_ZArith_BinIntDef_Z_div_eucl__curried(&'a self) -> &'a dyn Fn(i64) -> &'a dyn Fn(i64) -> __pair<i64, i64> {
  self.closure(move |a| {
    self.closure(move |b| {
      self.Coq_ZArith_BinIntDef_Z_div_eucl(
        a,
        b)
    })
  })
}

fn Coq_ZArith_BinIntDef_Z_div(&'a self, a: i64, b: i64) -> i64 {
  __pair_elim!(
    x, q, {
      x
    },
    self.Coq_ZArith_BinIntDef_Z_div_eucl(
      a,
      b))
}
fn Coq_ZArith_BinIntDef_Z_div__curried(&'a self) -> &'a dyn Fn(i64) -> &'a dyn Fn(i64) -> i64 {
  self.closure(move |a| {
    self.closure(move |b| {
      self.Coq_ZArith_BinIntDef_Z_div(
        a,
        b)
    })
  })
}

fn ConCert_Examples_Escrow_Escrow_buyer(&'a self, s: &'a ConCert_Examples_Escrow_Escrow_State<'a>) -> ConCert_Execution_Blockchain_Address<'a> {
  match s {
    &ConCert_Examples_Escrow_Escrow_State::build_state(_, last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) => {
      buyer
    },
  }
}
fn ConCert_Examples_Escrow_Escrow_buyer__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> ConCert_Execution_Blockchain_Address<'a> {
  self.closure(move |s| {
    self.ConCert_Examples_Escrow_Escrow_buyer(
      s)
  })
}

fn ConCert_Examples_Escrow_Escrow_last_action(&'a self, s: &'a ConCert_Examples_Escrow_Escrow_State<'a>) -> u64 {
  match s {
    &ConCert_Examples_Escrow_Escrow_State::build_state(_, last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) => {
      last_action
    },
  }
}
fn ConCert_Examples_Escrow_Escrow_last_action__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> u64 {
  self.closure(move |s| {
    self.ConCert_Examples_Escrow_Escrow_last_action(
      s)
  })
}

fn ConCert_Examples_Escrow_Escrow_seller(&'a self, s: &'a ConCert_Examples_Escrow_Escrow_State<'a>) -> ConCert_Execution_Blockchain_Address<'a> {
  match s {
    &ConCert_Examples_Escrow_Escrow_State::build_state(_, last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) => {
      seller
    },
  }
}
fn ConCert_Examples_Escrow_Escrow_seller__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> ConCert_Execution_Blockchain_Address<'a> {
  self.closure(move |s| {
    self.ConCert_Examples_Escrow_Escrow_seller(
      s)
  })
}

fn ConCert_Examples_Escrow_Escrow_seller_withdrawable(&'a self, s: &'a ConCert_Examples_Escrow_Escrow_State<'a>) -> ConCert_Execution_Blockchain_Amount<'a> {
  match s {
    &ConCert_Examples_Escrow_Escrow_State::build_state(_, last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) => {
      seller_withdrawable
    },
  }
}
fn ConCert_Examples_Escrow_Escrow_seller_withdrawable__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> ConCert_Execution_Blockchain_Amount<'a> {
  self.closure(move |s| {
    self.ConCert_Examples_Escrow_Escrow_seller_withdrawable(
      s)
  })
}

fn ConCert_Examples_Escrow_Escrow_buyer_withdrawable(&'a self, s: &'a ConCert_Examples_Escrow_Escrow_State<'a>) -> ConCert_Execution_Blockchain_Amount<'a> {
  match s {
    &ConCert_Examples_Escrow_Escrow_State::build_state(_, last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) => {
      buyer_withdrawable
    },
  }
}
fn ConCert_Examples_Escrow_Escrow_buyer_withdrawable__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> ConCert_Execution_Blockchain_Amount<'a> {
  self.closure(move |s| {
    self.ConCert_Examples_Escrow_Escrow_buyer_withdrawable(
      s)
  })
}

fn ConCert_Examples_Escrow_Escrow_set_State_last_action(&'a self, f: &'a dyn Fn(u64) -> u64, r: &'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a ConCert_Examples_Escrow_Escrow_State<'a> {
  self.alloc(
    ConCert_Examples_Escrow_Escrow_State::build_state(
      PhantomData,
      hint_app(f)(self.ConCert_Examples_Escrow_Escrow_last_action(
                    r)),
      self.ConCert_Examples_Escrow_Escrow_next_step(
        r),
      self.ConCert_Examples_Escrow_Escrow_seller(
        r),
      self.ConCert_Examples_Escrow_Escrow_buyer(
        r),
      self.ConCert_Examples_Escrow_Escrow_seller_withdrawable(
        r),
      self.ConCert_Examples_Escrow_Escrow_buyer_withdrawable(
        r)))
}
fn ConCert_Examples_Escrow_Escrow_set_State_last_action__curried(&'a self) -> &'a dyn Fn(&'a dyn Fn(u64) -> u64) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a ConCert_Examples_Escrow_Escrow_State<'a> {
  self.closure(move |f| {
    self.closure(move |r| {
      self.ConCert_Examples_Escrow_Escrow_set_State_last_action(
        f,
        r)
    })
  })
}

fn ConCert_Examples_Escrow_Escrow_set_State_next_step(&'a self, f: &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_NextStep<'a>) -> &'a ConCert_Examples_Escrow_Escrow_NextStep<'a>, r: &'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a ConCert_Examples_Escrow_Escrow_State<'a> {
  self.alloc(
    ConCert_Examples_Escrow_Escrow_State::build_state(
      PhantomData,
      self.ConCert_Examples_Escrow_Escrow_last_action(
        r),
      hint_app(f)(self.ConCert_Examples_Escrow_Escrow_next_step(
                    r)),
      self.ConCert_Examples_Escrow_Escrow_seller(
        r),
      self.ConCert_Examples_Escrow_Escrow_buyer(
        r),
      self.ConCert_Examples_Escrow_Escrow_seller_withdrawable(
        r),
      self.ConCert_Examples_Escrow_Escrow_buyer_withdrawable(
        r)))
}
fn ConCert_Examples_Escrow_Escrow_set_State_next_step__curried(&'a self) -> &'a dyn Fn(&'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_NextStep<'a>) -> &'a ConCert_Examples_Escrow_Escrow_NextStep<'a>) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a ConCert_Examples_Escrow_Escrow_State<'a> {
  self.closure(move |f| {
    self.closure(move |r| {
      self.ConCert_Examples_Escrow_Escrow_set_State_next_step(
        f,
        r)
    })
  })
}

fn ConCert_Examples_Escrow_Escrow_set_State_seller_withdrawable(&'a self, f: &'a dyn Fn(ConCert_Execution_Blockchain_Amount<'a>) -> ConCert_Execution_Blockchain_Amount<'a>, r: &'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a ConCert_Examples_Escrow_Escrow_State<'a> {
  self.alloc(
    ConCert_Examples_Escrow_Escrow_State::build_state(
      PhantomData,
      self.ConCert_Examples_Escrow_Escrow_last_action(
        r),
      self.ConCert_Examples_Escrow_Escrow_next_step(
        r),
      self.ConCert_Examples_Escrow_Escrow_seller(
        r),
      self.ConCert_Examples_Escrow_Escrow_buyer(
        r),
      hint_app(f)(self.ConCert_Examples_Escrow_Escrow_seller_withdrawable(
                    r)),
      self.ConCert_Examples_Escrow_Escrow_buyer_withdrawable(
        r)))
}
fn ConCert_Examples_Escrow_Escrow_set_State_seller_withdrawable__curried(&'a self) -> &'a dyn Fn(&'a dyn Fn(ConCert_Execution_Blockchain_Amount<'a>) -> ConCert_Execution_Blockchain_Amount<'a>) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a ConCert_Examples_Escrow_Escrow_State<'a> {
  self.closure(move |f| {
    self.closure(move |r| {
      self.ConCert_Examples_Escrow_Escrow_set_State_seller_withdrawable(
        f,
        r)
    })
  })
}

fn ConCert_Examples_Escrow_Escrow_set_State_buyer_withdrawable(&'a self, f: &'a dyn Fn(ConCert_Execution_Blockchain_Amount<'a>) -> ConCert_Execution_Blockchain_Amount<'a>, r: &'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a ConCert_Examples_Escrow_Escrow_State<'a> {
  self.alloc(
    ConCert_Examples_Escrow_Escrow_State::build_state(
      PhantomData,
      self.ConCert_Examples_Escrow_Escrow_last_action(
        r),
      self.ConCert_Examples_Escrow_Escrow_next_step(
        r),
      self.ConCert_Examples_Escrow_Escrow_seller(
        r),
      self.ConCert_Examples_Escrow_Escrow_buyer(
        r),
      self.ConCert_Examples_Escrow_Escrow_seller_withdrawable(
        r),
      hint_app(f)(self.ConCert_Examples_Escrow_Escrow_buyer_withdrawable(
                    r))))
}
fn ConCert_Examples_Escrow_Escrow_set_State_buyer_withdrawable__curried(&'a self) -> &'a dyn Fn(&'a dyn Fn(ConCert_Execution_Blockchain_Amount<'a>) -> ConCert_Execution_Blockchain_Amount<'a>) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a ConCert_Examples_Escrow_Escrow_State<'a> {
  self.closure(move |f| {
    self.closure(move |r| {
      self.ConCert_Examples_Escrow_Escrow_set_State_buyer_withdrawable(
        f,
        r)
    })
  })
}

fn Coq_Init_Nat_ltb(&'a self, a: u64, b: u64) -> bool { a < b }
fn Coq_Init_Nat_ltb__curried(&'a self) -> &'a dyn Fn(u64) -> &'a dyn Fn(u64) -> bool {
  self.closure(move |n| {
    self.closure(move |m| {
      self.Coq_Init_Nat_ltb(
        n,
        m)
    })
  })
}

fn Coq_Init_Nat_add(&'a self, a: u64, b: u64) -> u64 { a.checked_add(b).unwrap() }
fn Coq_Init_Nat_add__curried(&'a self) -> &'a dyn Fn(u64) -> &'a dyn Fn(u64) -> u64 {
  self.closure(move |n| {
    self.closure(move |m| {
      self.Coq_Init_Nat_add(
        n,
        m)
    })
  })
}

fn Coq_ZArith_BinIntDef_Z_gtb(&'a self, a: i64, b: i64) -> bool { a > b }
fn Coq_ZArith_BinIntDef_Z_gtb__curried(&'a self) -> &'a dyn Fn(i64) -> &'a dyn Fn(i64) -> bool {
  self.closure(move |x| {
    self.closure(move |y| {
      self.Coq_ZArith_BinIntDef_Z_gtb(
        x,
        y)
    })
  })
}


fn Coq_Init_Datatypes_andb__curried(&'a self) -> &'a dyn Fn(bool) -> &'a dyn Fn(bool) -> bool {
  self.closure(move |b1| {
    self.closure(move |b2| {
      __andb!(
        b1,
        b2)
    })
  })
}

fn ConCert_Examples_Escrow_Escrow_receive(&'a self, chain: &'a ConCert_Execution_Blockchain_Chain<'a>, ctx: &'a ConCert_Execution_Blockchain_ContractCallContext<'a>, state: &'a ConCert_Examples_Escrow_Escrow_State<'a>, msg: Option<&'a ConCert_Examples_Escrow_Escrow_Msg<'a>>) -> Result<__pair<&'a ConCert_Examples_Escrow_Escrow_State<'a>, &'a Coq_Init_Datatypes_list<'a, ActionBody<'a>>>, ConCert_Examples_Escrow_Escrow_Error<'a>> {
  match msg {
    Some(m) => {
      match m {
        &ConCert_Examples_Escrow_Escrow_Msg::commit_money(_) => {
          match self.ConCert_Examples_Escrow_Escrow_next_step(
                  state) {
            &ConCert_Examples_Escrow_Escrow_NextStep::buyer_commit(_) => {
              match self.ConCert_Execution_ResultMonad_result_of_option(
                      self.ConCert_Examples_Escrow_Escrow_subAmountOption(
                        self.ConCert_Execution_Blockchain_ctx_contract_balance(
                          ctx),
                        self.ConCert_Execution_Blockchain_ctx_amount(
                          ctx)),
                      self.ConCert_Examples_Escrow_Escrow_default_error()) {
                Ok(t) => {
                  let item_price =
                    self.Coq_ZArith_BinIntDef_Z_div(
                      t,
                      __Z_frompos(
                        __pos_zerobit(
                          1)));
                  let expected =
                    self.Coq_ZArith_BinIntDef_Z_mul(
                      item_price,
                      __Z_frompos(
                        __pos_zerobit(
                          1)));
                  match match self.ConCert_Execution_Blockchain_address_eqb()(
                                self.ConCert_Execution_Blockchain_ctx_from(
                                  ctx))(
                                self.ConCert_Examples_Escrow_Escrow_buyer(
                                  state)) {
                          true => {
                            Ok(
                              ())
                          },
                          false => {
                            Err(
                              self.ConCert_Examples_Escrow_Escrow_default_error())
                          },
                        } {
                    Ok(t2) => {
                      match match self.Coq_ZArith_BinIntDef_Z_eqb(
                                    self.ConCert_Execution_Blockchain_ctx_amount(
                                      ctx),
                                    expected) {
                              true => {
                                Ok(
                                  ())
                              },
                              false => {
                                Err(
                                  self.ConCert_Examples_Escrow_Escrow_default_error())
                              },
                            } {
                        Ok(t3) => {
                          Ok(
                            __mk_pair(
                              self.ConCert_Examples_Escrow_Escrow_set_State_last_action(
                                self.closure(move |x| {
                                  self.ConCert_Execution_Blockchain_current_slot(
                                    chain)
                                }),
                                self.ConCert_Examples_Escrow_Escrow_set_State_next_step(
                                  self.closure(move |x| {
                                    self.alloc(
                                      ConCert_Examples_Escrow_Escrow_NextStep::buyer_confirm(
                                        PhantomData))
                                  }),
                                  state)),
                              self.alloc(
                                Coq_Init_Datatypes_list::nil(
                                  PhantomData))))
                        },
                        Err(e) => {
                          Err(
                            e)
                        },
                      }
                    },
                    Err(e) => {
                      Err(
                        e)
                    },
                  }
                },
                Err(e) => {
                  Err(
                    e)
                },
              }
            },
            &ConCert_Examples_Escrow_Escrow_NextStep::buyer_confirm(_) => {
              Err(
                self.ConCert_Examples_Escrow_Escrow_default_error())
            },
            &ConCert_Examples_Escrow_Escrow_NextStep::withdrawals(_) => {
              Err(
                self.ConCert_Examples_Escrow_Escrow_default_error())
            },
            &ConCert_Examples_Escrow_Escrow_NextStep::no_next_step(_) => {
              Err(
                self.ConCert_Examples_Escrow_Escrow_default_error())
            },
          }
        },
        &ConCert_Examples_Escrow_Escrow_Msg::confirm_item_received(_) => {
          match self.ConCert_Examples_Escrow_Escrow_next_step(
                  state) {
            &ConCert_Examples_Escrow_Escrow_NextStep::buyer_commit(_) => {
              Err(
                self.ConCert_Examples_Escrow_Escrow_default_error())
            },
            &ConCert_Examples_Escrow_Escrow_NextStep::buyer_confirm(_) => {
              let item_price =
                self.Coq_ZArith_BinIntDef_Z_div(
                  self.ConCert_Execution_Blockchain_ctx_contract_balance(
                    ctx),
                  __Z_frompos(
                    __pos_zerobit(
                      __pos_zerobit(
                        1))));
              match match self.ConCert_Execution_Blockchain_address_eqb()(
                            self.ConCert_Execution_Blockchain_ctx_from(
                              ctx))(
                            self.ConCert_Examples_Escrow_Escrow_buyer(
                              state)) {
                      true => {
                        Ok(
                          ())
                      },
                      false => {
                        Err(
                          self.ConCert_Examples_Escrow_Escrow_default_error())
                      },
                    } {
                Ok(t) => {
                  match match self.Coq_ZArith_BinIntDef_Z_eqb(
                                self.ConCert_Execution_Blockchain_ctx_amount(
                                  ctx),
                                0) {
                          true => {
                            Ok(
                              ())
                          },
                          false => {
                            Err(
                              self.ConCert_Examples_Escrow_Escrow_default_error())
                          },
                        } {
                    Ok(t2) => {
                      let new_state =
                        self.ConCert_Examples_Escrow_Escrow_set_State_seller_withdrawable(
                          self.closure(move |x| {
                            self.Coq_ZArith_BinIntDef_Z_mul(
                              item_price,
                              __Z_frompos(
                                __pos_onebit(
                                  1)))
                          }),
                          self.ConCert_Examples_Escrow_Escrow_set_State_buyer_withdrawable(
                            self.closure(move |x| {
                              item_price
                            }),
                            self.ConCert_Examples_Escrow_Escrow_set_State_next_step(
                              self.closure(move |x| {
                                self.alloc(
                                  ConCert_Examples_Escrow_Escrow_NextStep::withdrawals(
                                    PhantomData))
                              }),
                              state)));
                      Ok(
                        __mk_pair(
                          new_state,
                          self.alloc(
                            Coq_Init_Datatypes_list::nil(
                              PhantomData))))
                    },
                    Err(e) => {
                      Err(
                        e)
                    },
                  }
                },
                Err(e) => {
                  Err(
                    e)
                },
              }
            },
            &ConCert_Examples_Escrow_Escrow_NextStep::withdrawals(_) => {
              Err(
                self.ConCert_Examples_Escrow_Escrow_default_error())
            },
            &ConCert_Examples_Escrow_Escrow_NextStep::no_next_step(_) => {
              Err(
                self.ConCert_Examples_Escrow_Escrow_default_error())
            },
          }
        },
        &ConCert_Examples_Escrow_Escrow_Msg::withdraw(_) => {
          match self.ConCert_Examples_Escrow_Escrow_next_step(
                  state) {
            &ConCert_Examples_Escrow_Escrow_NextStep::buyer_commit(_) => {
              match match self.Coq_ZArith_BinIntDef_Z_eqb(
                            self.ConCert_Execution_Blockchain_ctx_amount(
                              ctx),
                            0) {
                      true => {
                        Ok(
                          ())
                      },
                      false => {
                        Err(
                          self.ConCert_Examples_Escrow_Escrow_default_error())
                      },
                    } {
                Ok(t) => {
                  match match self.Coq_Init_Nat_ltb(
                                self.Coq_Init_Nat_add(
                                  self.ConCert_Examples_Escrow_Escrow_last_action(
                                    state),
                                  __nat_succ(
                                    __nat_succ(
                                      __nat_succ(
                                        __nat_succ(
                                          __nat_succ(
                                            __nat_succ(
                                              __nat_succ(
                                                __nat_succ(
                                                  __nat_succ(
                                                    __nat_succ(
                                                      __nat_succ(
                                                        __nat_succ(
                                                          __nat_succ(
                                                            __nat_succ(
                                                              __nat_succ(
                                                                __nat_succ(
                                                                  __nat_succ(
                                                                    __nat_succ(
                                                                      __nat_succ(
                                                                        __nat_succ(
                                                                          __nat_succ(
                                                                            __nat_succ(
                                                                              __nat_succ(
                                                                                __nat_succ(
                                                                                  __nat_succ(
                                                                                    __nat_succ(
                                                                                      __nat_succ(
                                                                                        __nat_succ(
                                                                                          __nat_succ(
                                                                                            __nat_succ(
                                                                                              __nat_succ(
                                                                                                __nat_succ(
                                                                                                  __nat_succ(
                                                                                                    __nat_succ(
                                                                                                      __nat_succ(
                                                                                                        __nat_succ(
                                                                                                          __nat_succ(
                                                                                                            __nat_succ(
                                                                                                              __nat_succ(
                                                                                                                __nat_succ(
                                                                                                                  __nat_succ(
                                                                                                                    __nat_succ(
                                                                                                                      __nat_succ(
                                                                                                                        __nat_succ(
                                                                                                                          __nat_succ(
                                                                                                                            __nat_succ(
                                                                                                                              __nat_succ(
                                                                                                                                __nat_succ(
                                                                                                                                  __nat_succ(
                                                                                                                                    __nat_succ(
                                                                                                                                      0))))))))))))))))))))))))))))))))))))))))))))))))))),
                                self.ConCert_Execution_Blockchain_current_slot(
                                  chain)) {
                          true => {
                            Err(
                              self.ConCert_Examples_Escrow_Escrow_default_error())
                          },
                          false => {
                            Ok(
                              ())
                          },
                        } {
                    Ok(t2) => {
                      match match self.ConCert_Execution_Blockchain_address_eqb()(
                                    self.ConCert_Execution_Blockchain_ctx_from(
                                      ctx))(
                                    self.ConCert_Examples_Escrow_Escrow_seller(
                                      state)) {
                              true => {
                                Ok(
                                  ())
                              },
                              false => {
                                Err(
                                  self.ConCert_Examples_Escrow_Escrow_default_error())
                              },
                            } {
                        Ok(t3) => {
                          let balance =
                            self.ConCert_Execution_Blockchain_ctx_contract_balance(
                              ctx);
                          Ok(
                            __mk_pair(
                              self.ConCert_Examples_Escrow_Escrow_set_State_next_step(
                                self.closure(move |x| {
                                  self.alloc(
                                    ConCert_Examples_Escrow_Escrow_NextStep::no_next_step(
                                      PhantomData))
                                }),
                                state),
                              self.alloc(
                                Coq_Init_Datatypes_list::cons(
                                  PhantomData,
                                  ActionBody::Transfer(
                                    self.ConCert_Examples_Escrow_Escrow_seller(
                                      state),
                                    balance),
                                  self.alloc(
                                    Coq_Init_Datatypes_list::nil(
                                      PhantomData))))))
                        },
                        Err(e) => {
                          Err(
                            e)
                        },
                      }
                    },
                    Err(e) => {
                      Err(
                        e)
                    },
                  }
                },
                Err(e) => {
                  Err(
                    e)
                },
              }
            },
            &ConCert_Examples_Escrow_Escrow_NextStep::buyer_confirm(_) => {
              Err(
                self.ConCert_Examples_Escrow_Escrow_default_error())
            },
            &ConCert_Examples_Escrow_Escrow_NextStep::withdrawals(_) => {
              match match self.Coq_ZArith_BinIntDef_Z_eqb(
                            self.ConCert_Execution_Blockchain_ctx_amount(
                              ctx),
                            0) {
                      true => {
                        Ok(
                          ())
                      },
                      false => {
                        Err(
                          self.ConCert_Examples_Escrow_Escrow_default_error())
                      },
                    } {
                Ok(t) => {
                  let from =
                    self.ConCert_Execution_Blockchain_ctx_from(
                      ctx);
                  match match self.ConCert_Execution_Blockchain_address_eqb()(
                                from)(
                                self.ConCert_Examples_Escrow_Escrow_buyer(
                                  state)) {
                          true => {
                            Ok(
                              __mk_pair(
                                self.ConCert_Examples_Escrow_Escrow_buyer_withdrawable(
                                  state),
                                self.ConCert_Examples_Escrow_Escrow_set_State_buyer_withdrawable(
                                  self.closure(move |x| {
                                    0
                                  }),
                                  state)))
                          },
                          false => {
                            match self.ConCert_Execution_Blockchain_address_eqb()(
                                    from)(
                                    self.ConCert_Examples_Escrow_Escrow_seller(
                                      state)) {
                              true => {
                                Ok(
                                  __mk_pair(
                                    self.ConCert_Examples_Escrow_Escrow_seller_withdrawable(
                                      state),
                                    self.ConCert_Examples_Escrow_Escrow_set_State_seller_withdrawable(
                                      self.closure(move |x| {
                                        0
                                      }),
                                      state)))
                              },
                              false => {
                                Err(
                                  self.ConCert_Examples_Escrow_Escrow_default_error())
                              },
                            }
                          },
                        } {
                    Ok(t2) => {
                      __pair_elim!(
                        new_state, to_pay, {
                          match match self.Coq_ZArith_BinIntDef_Z_gtb(
                                        new_state,
                                        0) {
                                  true => {
                                    Ok(
                                      ())
                                  },
                                  false => {
                                    Err(
                                      self.ConCert_Examples_Escrow_Escrow_default_error())
                                  },
                                } {
                            Ok(t3) => {
                              let new_state2 =
                                match __andb!(
                                        self.Coq_ZArith_BinIntDef_Z_eqb(
                                          self.ConCert_Examples_Escrow_Escrow_buyer_withdrawable(
                                            to_pay),
                                          0),
                                        self.Coq_ZArith_BinIntDef_Z_eqb(
                                          self.ConCert_Examples_Escrow_Escrow_seller_withdrawable(
                                            to_pay),
                                          0)) {
                                  true => {
                                    self.ConCert_Examples_Escrow_Escrow_set_State_next_step(
                                      self.closure(move |x| {
                                        self.alloc(
                                          ConCert_Examples_Escrow_Escrow_NextStep::no_next_step(
                                            PhantomData))
                                      }),
                                      to_pay)
                                  },
                                  false => {
                                    to_pay
                                  },
                                };
                              Ok(
                                __mk_pair(
                                  new_state2,
                                  self.alloc(
                                    Coq_Init_Datatypes_list::cons(
                                      PhantomData,
                                      ActionBody::Transfer(
                                        self.ConCert_Execution_Blockchain_ctx_from(
                                          ctx),
                                        new_state),
                                      self.alloc(
                                        Coq_Init_Datatypes_list::nil(
                                          PhantomData))))))
                            },
                            Err(e) => {
                              Err(
                                e)
                            },
                          }
                        },
                        t2)
                    },
                    Err(e) => {
                      Err(
                        e)
                    },
                  }
                },
                Err(e) => {
                  Err(
                    e)
                },
              }
            },
            &ConCert_Examples_Escrow_Escrow_NextStep::no_next_step(_) => {
              Err(
                self.ConCert_Examples_Escrow_Escrow_default_error())
            },
          }
        },
      }
    },
    None => {
      Err(
        self.ConCert_Examples_Escrow_Escrow_default_error())
    },
  }
}
fn ConCert_Examples_Escrow_Escrow_receive__curried(&'a self) -> &'a dyn Fn(&'a ConCert_Execution_Blockchain_Chain<'a>) -> &'a dyn Fn(&'a ConCert_Execution_Blockchain_ContractCallContext<'a>) -> &'a dyn Fn(&'a ConCert_Examples_Escrow_Escrow_State<'a>) -> &'a dyn Fn(Option<&'a ConCert_Examples_Escrow_Escrow_Msg<'a>>) -> Result<__pair<&'a ConCert_Examples_Escrow_Escrow_State<'a>, &'a Coq_Init_Datatypes_list<'a, ActionBody<'a>>>, ConCert_Examples_Escrow_Escrow_Error<'a>> {
  self.closure(move |chain| {
    self.closure(move |ctx| {
      self.closure(move |state| {
        self.closure(move |msg| {
          self.ConCert_Examples_Escrow_Escrow_receive(
            chain,
            ctx,
            state,
            msg)
        })
      })
    })
  })
}
}

#[init(contract = "escrow", payable, enable_logger, low_level)]
fn contract_init<StateError: Default>(
    ctx: &impl HasInitContext<()>,
    amount: concordium_std::Amount,
    logger: &mut impl HasLogger,
    state: &mut impl HasContractState<StateError>
) -> Result<(), InitError> {
    let prg = Program::new();
    let params =
        match <_>::concert_deserial(&mut ctx.parameter_cursor(), &prg.__alloc) {
            Ok(p) => p,
            Err(_) => return Err(InitError::DeserialParams)
        };
    let cchain =
        ConCert_Execution_Blockchain_Chain::build_chain(
            PhantomData,
            0, // No chain height
            ctx.metadata().slot_time().timestamp_millis(),
            0 // No finalized height
        );
    let cctx =
        ConCert_Execution_Blockchain_ContractCallContext::build_ctx(
            PhantomData,
            Address::Account(ctx.init_origin()),
            Address::Account(ctx.init_origin()),
            Address::Contract(ContractAddress { index: 0, subindex: 0 }),
            amount.micro_ccd as i64,
            amount.micro_ccd as i64);
    let res = prg.ConCert_Examples_Escrow_Escrow_init(&cchain, &cctx, params);
    match res {
        Result::Ok(init_state) => {
            match init_state.concert_serial(state) {
                Ok(_) => Ok(()),
                Err(_) => Err(InitError::SerialParams)
            }
        }
        Result::Err(error) => Err(InitError::Error(error))
    }
}

fn convert_actions<A: HasActions>(acts: &Coq_Init_Datatypes_list<ActionBody>) -> Result<A, ReceiveError> {
  match acts {
    &Coq_Init_Datatypes_list::nil(_) => Ok(A::accept()),
    &Coq_Init_Datatypes_list::cons(_, hd, tl) => {
      let cact =
        if let ActionBody::Transfer(Address::Account(acc), amount) = hd {
          let amount = convert::TryInto::try_into(amount).map_err(|_| ReceiveError::ConvertActions)?;
          A::simple_transfer(&acc, concordium_std::Amount::from_micro_ccd(amount))
        } else {
          return Err(ReceiveError::ConvertActions) // Cannot handle call to contract through ConCert, use Concordium functions instead
        };
      Ok(cact.and_then(convert_actions(tl)?))
    }
  }
}

#[receive(contract = "escrow", name = "ConCert_Examples_Escrow_Escrow_receive", payable, enable_logger, low_level)]
fn contract_receive<A: HasActions, StateError: Default>(
    ctx: &impl HasReceiveContext<()>,
    amount: concordium_std::Amount,
    logger: &mut impl HasLogger,
    state: &mut impl HasContractState<StateError>,
) -> Result<A, ReceiveError> {
    let prg = Program::new();
    let msg =
        match <_>::concert_deserial(&mut ctx.parameter_cursor(), &prg.__alloc) {
            Ok(m) => m,
            Err(_) => return Err(ReceiveError::DeserialMsg)
        };
    let old_state =
        match <_>::concert_deserial(state, &prg.__alloc) {
            Ok(s) => s,
            Err(_) => return Err(ReceiveError::DeserialOldState)
        };
    let cchain =
        ConCert_Execution_Blockchain_Chain::build_chain(
            PhantomData,
            0, // No chain height
            ctx.metadata().slot_time().timestamp_millis(),
            0 // No finalized height
        );
    let balance = if ctx.sender() != concordium_std::Address::Contract(ctx.self_address()) {
   // if the contract is not calling itself, we add amount to the current balance
   // as expeced by the ConCert execution model
   (ctx.self_balance().micro_ccd + amount.micro_ccd) as i64
    } else {
        ctx.self_balance().micro_ccd as i64
    };
    let cctx =
        ConCert_Execution_Blockchain_ContractCallContext::build_ctx(
            PhantomData,
            Address::Account(ctx.invoker()),
            ctx.sender(),
            Address::Contract(ctx.self_address()),
            balance,
            amount.micro_ccd as i64);
    let res = prg.ConCert_Examples_Escrow_Escrow_receive(&cchain, &cctx, old_state, msg);
    match res {
        Result::Ok((new_state, acts)) => {
            state.truncate(0);
            match new_state.concert_serial(state) {
                Ok(_) => convert_actions(acts),
                Err(_) => Err(ReceiveError::SerialNewState)
            }
        }
        Result::Err(error) => Err(ReceiveError::Error(error))
    }
}
// END OF THE GENERATED PART

// The tests are written manually
#[concordium_cfg_test]
mod tests {
    use super::*;
    use concordium_std::test_infrastructure::*;
    use std::io::Write;

    // This test writes the init data and reads it back.
    // Uncomment this test if you want to write the binary
    // data for making an actual call with concordium-client
    // NOTE: once uncommented, run is as [cargo test], so
    // it can write into a file
   // #[concordium_test]
    fn test_read_init_data() {
        let buyer_addr = AccountAddress([0u8; 32]);
        let setup =
            ConCert_Examples_Escrow_Escrow_Setup::build_setup(PhantomData,
                                                                 Address::Account(buyer_addr));
        match std::fs::File::create("escrow_init_data.bin") {
            Ok(mut f) => {
                let param_bytes = concert_std::to_bytes(&setup);
                match f.write_all(&param_bytes) {
                    Ok(_) =>
                        match std::fs::read("escrow_init_data.bin") {
                            Ok(v) => {
                                let prg = Program::new();
                                let mut cur = Cursor::new(v);
                                match <ConCert_Examples_Escrow_Escrow_Setup>::concert_deserial(&mut cur, &prg.__alloc) {
                                    Ok(p) => {
                                        claim_eq!(p, setup, "Wrong deserial result");
                                    },
                                    Err(e) => fail!("{:?}", e)
                                };
                            },
                            Err(e) => fail!("{:?}", e)
                        },
                    Err(e) => fail!("Cannot write file: {:?}", e)
                }
            },
            Err(e) => fail!("Cannot create file: {:?}", e)
        }
    }

    #[concordium_test]
    fn test_init() {
        // buyer_addr should be != seller_addr
        let buyer_addr = AccountAddress([0u8; 32]);
        let seller_addr = AccountAddress([1u8; 32]);
        let amount = 10;
        let data = Vec::new();
        let mut st = ContractStateTest::open(data);
        let mut ctx = InitContextTest::empty();
        let setup =
            ConCert_Examples_Escrow_Escrow_Setup::build_setup(PhantomData,
                                                                 Address::Account(buyer_addr));
        let param_bytes = concert_std::to_bytes(&setup);
        ctx.set_parameter(&param_bytes);
        let slot_time = Timestamp::from_timestamp_millis(11);
        ctx.set_metadata_slot_time(slot_time);
        ctx.set_init_origin(seller_addr);


        // set up the logger so we can intercept and analyze them at the end.
        let mut logger = LogRecorder::init();

        // call the init function
        // amount must be even for init to succeed
        let out = contract_init(&ctx, Amount::from_micro_ccd(amount), &mut logger, &mut st);

        let res = match out {
            Ok(res) => res,
            Err(e) => fail!("Contract initialization failed: {:?}", e),
        };
        claim_eq!(res, ());
        let arena = bumpalo::Bump::new();
        st.seek(SeekFrom::Start(0)).expect("Seek failed");
        let deserial_state : ConCert_Examples_Escrow_Escrow_State = <_>::concert_deserial(&mut st, &arena).expect("Deserialisation failed");
        let res = match deserial_state {
            ConCert_Examples_Escrow_Escrow_State::build_state(_, last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) => {
                claim_eq!(last_action, slot_time.timestamp_millis(), "Wrong last_action:{:?}",last_action);
                match next_step {
                    ConCert_Examples_Escrow_Escrow_NextStep::buyer_commit(PhantomData) => (),
                    _ => fail!("Wrong next_step"),
                }
                match seller {
                    Address::Account(a) => {
                        claim_eq!(a, seller_addr, "Wrong seller: {:?}", a);
                    },
                    _ => fail!("Not an account")
                };
                match buyer {
                    Address::Account(a) => {
                        claim_eq!(a, buyer_addr, "Wrong buyer: {:?}", a);
                    },
                    _ => fail!("Not an account")
                };
                claim_eq!(seller_withdrawable, 0, "Wrong seller_withdrawable: {:?}", seller_withdrawable);
                claim_eq!(buyer_withdrawable, 0, "Wrong buyer_withdrawable: {:?}", buyer_withdrawable);
            },
        };
    }

    #[concordium_test]
    fn test_receive() {
        let mut ctx = ReceiveContextTest::empty();
        let prg = Program::new();
        let mut data : Vec<u8> = Vec::new();
        let params = Option::Some(&ConCert_Examples_Escrow_Escrow_Msg::commit_money(PhantomData));
        params.concert_serial(&mut data).expect("Serialisation failed");
        ctx.set_parameter(&data);
        let data_st : Vec<u8> = Vec::new();
        let buyer_addr = AccountAddress([0u8; 32]);
        let seller_addr = AccountAddress([1u8; 32]);
        let self_addr = ContractAddress {index: 0, subindex : 0};
        let amount = 10;
        let init_balance = 10;
        let slot_time = Timestamp::from_timestamp_millis(11);
        ctx.set_metadata_slot_time(slot_time);
        ctx.set_invoker(buyer_addr);
        ctx.set_sender(Address::Account(buyer_addr));
        ctx.set_self_address(self_addr);
        ctx.set_self_balance(concordium_std::Amount::from_micro_ccd(init_balance));
        let mut st = ContractStateTest::open(data_st);
        let v : ConCert_Examples_Escrow_Escrow_State =
            ConCert_Examples_Escrow_Escrow_State::build_state
            (PhantomData,
             0,
             &ConCert_Examples_Escrow_Escrow_NextStep::buyer_commit(PhantomData),
             Address::Account(seller_addr),
             Address::Account(buyer_addr),
             0,
             0);
        v.concert_serial(&mut st).expect("Serialisation of failed");
        st.seek(SeekFrom::Start(0)).expect("Seek failed");
        // set up the logger so we can intercept and analyze them at the end.
        let mut logger = LogRecorder::init();
        let res: Result<ActionsTree, _> =
            contract_receive(&ctx, Amount::from_micro_ccd(amount), &mut logger, &mut st);
        let actions = match res {
            Err(e) => fail!("Contract receive failed, but it should not have: {:?}",e),
            Ok(actions) => actions,
        };
        claim_eq!(actions, ActionsTree::Accept, "Contract receive produced incorrect actions.");
        let arena = bumpalo::Bump::new();
        st.seek(SeekFrom::Start(0)).expect("Seek failed");
        let deserial_state : ConCert_Examples_Escrow_Escrow_State = <_>::concert_deserial(&mut st, &arena).expect("Deserialisation failed");
        let res = match deserial_state {
            ConCert_Examples_Escrow_Escrow_State::build_state(_, last_action, next_step, seller, buyer, seller_withdrawable, buyer_withdrawable) => {
                claim_eq!(last_action, slot_time.timestamp_millis(), "Wrong last_action:{:?}",last_action);
                match next_step {
                    ConCert_Examples_Escrow_Escrow_NextStep::buyer_confirm(PhantomData) => (),
                    _ => fail!("Wrong next_step"),
                }
                match seller {
                    Address::Account(a) => {
                        claim_eq!(a, seller_addr, "Wrong seller: {:?}", a);
                    },
                    _ => fail!("Not an account")
                };
                match buyer {
                    Address::Account(a) => {
                        claim_eq!(a, buyer_addr, "Wrong buyer: {:?}", a);
                    },
                    _ => fail!("Not an account")
                };
                claim_eq!(seller_withdrawable, 0, "Wrong seller_withdrawable: {:?}", seller_withdrawable);
                claim_eq!(buyer_withdrawable, 0, "Wrong buyer_withdrawable: {:?}", buyer_withdrawable);
            },
        };
    }
}
