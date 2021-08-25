module SafePredPartial exposing (..)
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

conCert_Extraction_Examples_ElmExtractExamples_ElmExamples_safe_pred_partial_cert_pass : () -> Sig Nat
conCert_Extraction_Examples_ElmExtractExamples_ElmExamples_safe_pred_partial_cert_pass not_zero =
  safe_pred (S O)
main = Html.text (Debug.toString (Expect.equal (conCert_Extraction_Examples_ElmExtractExamples_ElmExamples_safe_pred_partial_cert_pass ()) (Exist O)))
suite = Test.test (Debug.toString 1)(\ _ -> Expect.equal (conCert_Extraction_Examples_ElmExtractExamples_ElmExamples_safe_pred_partial_cert_pass ()) (Exist O))
