module Increment exposing (..)
import Test
import Html
import Expect exposing (Expectation)
type Nat
  = O
  | S Nat

type Sig a
  = Exist a

add : Nat -> Nat -> Nat
add n m =
  case n of
    O ->
      m
    S p ->
      S (add p m)

proj1_sig : Sig a -> a
proj1_sig e =
  case e of
    Exist a ->
      a

inc_counter : Nat -> Sig Nat -> Sig Nat
inc_counter st inc =
  Exist (add st (proj1_sig inc))
main = Html.text (Debug.toString (Expect.equal (inc_counter O (Exist (S O))) (Exist (S O))))
suite = Test.test (Debug.toString 1)(\ _ -> Expect.equal (inc_counter O (Exist (S O))) (Exist (S O)))
