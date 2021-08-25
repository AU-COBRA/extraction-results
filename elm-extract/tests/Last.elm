module Last exposing (..)
import Test
import Html
import Expect exposing (Expectation)
type List a
  = Nil
  | Cons a (List a)

last : List a -> a -> a
last l d =
  case l of
    Nil ->
      d
    Cons a l2 ->
      case l2 of
        Nil ->
          a
        Cons a0 l0 ->
          last l2 d
main = Html.text (Debug.toString (Expect.equal (last (Cons 1 (Cons 10 Nil)) 0) 10))
suite = Test.test (Debug.toString 1)(\ _ -> Expect.equal (last (Cons 1 (Cons 10 Nil)) 0) 10)
