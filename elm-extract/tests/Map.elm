module Map exposing (..)
import Test
import Html
import Expect exposing (Expectation)
type List a
  = Nil
  | Cons a (List a)

map : (a -> b) -> List a -> List b
map f =
  let
    map2 l =
      case l of
        Nil ->
          Nil
        Cons a t ->
          Cons (f a) (map2 t)
  in
  map2
main = Html.text (Debug.toString (Expect.equal (map (\x->x+1) (Cons 1 (Cons 0 Nil))) (Cons 2 (Cons 1 Nil))))
suite = Test.test (Debug.toString 1)(\ _ -> Expect.equal (map (\x->x+1) (Cons 1 (Cons 0 Nil))) (Cons 2 (Cons 1 Nil)))
