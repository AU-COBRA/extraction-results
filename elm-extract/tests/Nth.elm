module Nth exposing (..)
import Test
import Html
import Expect exposing (Expectation)
type Nat
  = O
  | S Nat

type List a
  = Nil
  | Cons a (List a)

nth : Nat -> List a -> a -> a
nth n l default =
  case n of
    O ->
      case l of
        Nil ->
          default
        Cons x l2 ->
          x
    S m ->
      case l of
        Nil ->
          default
        Cons x t ->
          nth m t default
main = Html.text (Debug.toString (Expect.equal (nth O (Cons 1 (Cons 0 Nil)) 0) 1))
suite = Test.test (Debug.toString 1)(\ _ -> Expect.equal (nth O (Cons 1 (Cons 0 Nil)) 0) 1)
