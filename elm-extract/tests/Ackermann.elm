module Ackermann exposing (..)
import Test
import Html
import Expect exposing (Expectation)
type Prod a b
  = Pair a b

type Nat
  = O
  | S Nat

ackermann : Prod Nat Nat -> Nat
ackermann x =
  (let
     fix_F_sub x2 r =
       (case x2 of
          Pair n b ->
            case n of
              O ->
                \heq_ab -> (let
                              add n2 m =
                                case n2 of
                                  O ->
                                    m
                                  S p ->
                                    S (add p m)
                            in
                            add) b (S O)
              S a ->
                case b of
                  O ->
                    \heq_ab -> fix_F_sub (Pair a (S O)) ()
                  S b2 ->
                    \heq_ab -> fix_F_sub (Pair a (fix_F_sub (Pair (S a) b2) ())) ()) ()
   in
   fix_F_sub) x ()
main = Html.text (Debug.toString (Expect.equal (ackermann (Pair (S (S (S O))) (S (S (S O))))) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
suite = Test.test (Debug.toString 1)(\ _ -> Expect.equal (ackermann (Pair (S (S (S O))) (S (S (S O))))) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
