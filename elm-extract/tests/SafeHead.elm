module SafeHead exposing (..)
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

type List a
  = Nil
  | Cons a (List a)

proj1_sig : Sig a -> a
proj1_sig e =
  case e of
    Exist a ->
      a

false_rect : () -> p
false_rect p =
  false_rec ()

safe_head : Sig (List a) -> a
safe_head non_empty_list =
  (case proj1_sig non_empty_list of
     Nil ->
       \x -> false_rect ()
     Cons hd tl ->
       \x -> hd) ()

repeat : a -> Nat -> List a
repeat x n =
  case n of
    O ->
      Nil
    S k ->
      Cons x (repeat x k)

add : Nat -> Nat -> Nat
add n m =
  case n of
    O ->
      m
    S p ->
      S (add p m)

head_of_repeat_plus_one : Nat -> a -> a
head_of_repeat_plus_one n a =
  safe_head (Exist (repeat a (add (S O) n)))
main = Html.text (Debug.toString (Expect.equal (head_of_repeat_plus_one (S O) 1) 1))
suite = Test.test (Debug.toString 1)(\ _ -> Expect.equal (head_of_repeat_plus_one (S O) 1) 1)
