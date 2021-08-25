module SafePredFull exposing (..)
import Test
import Html
import Expect exposing (Expectation)
false_rec : () -> a
false_rec _ = false_rec ()
type Nat
  = O
  | S Nat

type Sig a
  = Exist a

false_rect : () -> p
false_rect p =
  false_rec ()

safe_pred : Nat -> Sig Nat
safe_pred n =
  (case n of
     O ->
       \heq h -> false_rect ()
     S m ->
       \heq h -> Exist m) () ()

safe_pred_full : Sig Nat
safe_pred_full =
  safe_pred (S O)
main = Html.text (Debug.toString (Expect.equal safe_pred_full (Exist O)))
suite = Test.test (Debug.toString 1)(\ _ -> Expect.equal safe_pred_full (Exist O))
