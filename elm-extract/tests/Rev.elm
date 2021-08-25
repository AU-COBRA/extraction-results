module Rev exposing (..)
import Test
import Html
import Expect exposing (Expectation)
type List a
  = Nil
  | Cons a (List a)

app : List a -> List a -> List a
app l m =
  case l of
    Nil ->
      m
    Cons a l1 ->
      Cons a (app l1 m)

rev : List a -> List a
rev l =
  case l of
    Nil ->
      Nil
    Cons x l2 ->
      app (rev l2) (Cons x Nil)
main = Html.text (Debug.toString (Expect.equal (rev (Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil))))) (Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil))))))
suite = Test.test (Debug.toString 1)(\ _ -> Expect.equal (rev (Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil))))) (Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil)))))
