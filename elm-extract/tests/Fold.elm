module Fold exposing (..)
import Test
import Html
import Expect exposing (Expectation)
type List a
  = Nil
  | Cons a (List a)

fold_left : (a -> b -> a) -> List b -> a -> a
fold_left f =
  let
    fold_left2 l a0 =
      case l of
        Nil ->
          a0
        Cons b t ->
          fold_left2 t (f a0 b)
  in
  fold_left2
main = Html.text (Debug.toString ((Expect.equal (fold_left (+) (Cons 1 (Cons 0 Nil)) 0)) 1))
suite = Test.test (Debug.toString 1)(\ _ -> (Expect.equal (fold_left (+) (Cons 1 (Cons 0 Nil)) 0)) 1)
