module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

type alias Prod_ a b = (a,b)

string_eq : String -> String -> Bool
string_eq s1 s2 = s1 == s2

succ : Int -> Int
succ n = n + 1

nat_le : Int -> Int -> Bool
nat_le n1 n2 = n1 <= n2


false_rec : () -> a
false_rec _ = false_rec ()


type Msg
  = MsgName String
  | MsgPassword String
  | MsgPasswordAgain String

type StorageMsg
  = Add
  | UpdateEntry Msg

type Sig a
  = Exist a

type StoredEntry
  = Build_StoredEntry String String

seName : StoredEntry -> String
seName s =
  case s of
    Build_StoredEntry seName2 sePassword ->
      seName2

type alias ValidStoredEntry = Sig StoredEntry

proj1_sig : Sig a -> a
proj1_sig e =
  case e of
    Exist a ->
      a

seNames : List ValidStoredEntry -> List String
seNames l =
  List.map (\x -> seName (proj1_sig x)) l

type Entry
  = Build_Entry String String String

type Model
  = Build_Model (Sig (List ValidStoredEntry)) (List String) Entry

orb : Bool -> Bool -> Bool
orb b1 b2 =
  case b1 of
    True ->
      True
    False ->
      b2

existsb : (a -> Bool) -> List a -> Bool
existsb f =
  let
    existsb2 l =
      case l of
        [] ->
          False
        a :: l2 ->
          orb (f a) (existsb2 l2)
  in
  existsb2

name : Entry -> String
name e =
  case e of
    Build_Entry name2 password2 passwordAgain2 ->
      name2

currentEntry : Model -> Entry
currentEntry m =
  case m of
    Build_Model users2 errors2 currentEntry2 ->
      currentEntry2

users : Model -> Sig (List ValidStoredEntry)
users m =
  case m of
    Build_Model users2 errors2 currentEntry2 ->
      users2

password : Entry -> String
password e =
  case e of
    Build_Entry name2 password2 passwordAgain2 ->
      password2

passwordAgain : Entry -> String
passwordAgain e =
  case e of
    Build_Entry name2 password2 passwordAgain2 ->
      passwordAgain2

validateModel : Model -> List String
validateModel model2 =
  let
    res =
      (::) (Tuple.pair (not (existsb (\nm -> string_eq nm (name (currentEntry model2))) (seNames (proj1_sig (users model2))))) "User already exists!") ((::) (Tuple.pair (not (string_eq (name (currentEntry model2)) "")) "Empty name!") ((::) (Tuple.pair (string_eq (password (currentEntry model2)) (passwordAgain (currentEntry model2))) "Passwords do not match!") ((::) (Tuple.pair (nat_le (succ (succ (succ (succ (succ (succ (succ (succ 0)))))))) (String.length (password (currentEntry model2)))) "Password is too short!") [])))
  in
  List.map Tuple.second (List.filter (\x -> not (Tuple.first x)) res)

type alias ValidEntry = Sig Entry

toValidStoredEntry : ValidEntry -> ValidStoredEntry
toValidStoredEntry entry2 =
  Exist (Build_StoredEntry (name (proj1_sig entry2)) (password (proj1_sig entry2)))

errors : Model -> List String
errors m =
  case m of
    Build_Model users2 errors2 currentEntry2 ->
      errors2

set_Model_users : (Sig (List ValidStoredEntry) -> Sig (List ValidStoredEntry)) -> Model -> Model
set_Model_users f r =
  Build_Model (f (users r)) (errors r) (currentEntry r)

set_Model_errors : (List String -> List String) -> Model -> Model
set_Model_errors f r =
  Build_Model (users r) (f (errors r)) (currentEntry r)

set_Model_currentEntry : (Entry -> Entry) -> Model -> Model
set_Model_currentEntry f r =
  Build_Model (users r) (errors r) (f (currentEntry r))

set_Entry_name : (String -> String) -> Entry -> Entry
set_Entry_name f r =
  Build_Entry (f (name r)) (password r) (passwordAgain r)

set_Entry_password : (String -> String) -> Entry -> Entry
set_Entry_password f r =
  Build_Entry (name r) (f (password r)) (passwordAgain r)

set_Entry_passwordAgain : (String -> String) -> Entry -> Entry
set_Entry_passwordAgain f r =
  Build_Entry (name r) (password r) (f (passwordAgain r))

updateEntry : Msg -> Entry -> Entry
updateEntry msg2 model2 =
  case msg2 of
    MsgName newName ->
      set_Entry_name (\x -> newName) model2
    MsgPassword newPassword ->
      set_Entry_password (\x -> newPassword) model2
    MsgPasswordAgain newPassword ->
      set_Entry_passwordAgain (\x -> newPassword) model2

updateModel : StorageMsg -> Model -> Prod_ Model (Cmd StorageMsg)
updateModel msg2 model2 =
  let
    program_branch_0 heq_msg =
      let
        filtered_var =
          validateModel model2
      in
      let
        program_branch_02 heq_anonymous =
          let
            validEntry2 =
              Exist (currentEntry model2)
          in
          let
            newValidStoredEntry =
              toValidStoredEntry validEntry2
          in
          let
            newList =
              (::) newValidStoredEntry (proj1_sig (users model2))
          in
          Tuple.pair (set_Model_users (\x -> Exist newList) model2) Cmd.none
      in
      let
        program_branch_1 errs x heq_anonymous =
          Tuple.pair (set_Model_errors (\x2 -> errs) model2) Cmd.none
      in
      (case filtered_var of
         [] ->
           program_branch_02
         s :: l ->
           program_branch_1 ((::) s l) ()) ()
  in
  let
    program_branch_1 entryMsg heq_msg =
      Tuple.pair (set_Model_currentEntry (\x -> updateEntry entryMsg (currentEntry model2)) model2) Cmd.none
  in
  (case msg2 of
     Add ->
       program_branch_0
     UpdateEntry entryMsg ->
       program_branch_1 entryMsg) ()

initModel : Prod_ Model (Cmd StorageMsg)
initModel =
  let
    entry2 =
      Build_Model (Exist []) [] (Build_Entry "" "" "")
  in
  Tuple.pair entry2 Cmd.none

-- Manually appended VIEWS

view : Model -> Html StorageMsg
view model =
  div []
    [ viewInput "text" "Name" (name (currentEntry model)) (UpdateEntry << MsgName)
    , viewInput "password" "Password" (password (currentEntry model)) (UpdateEntry << MsgPassword)
    , viewInput "password" "Re-enter Password" (passwordAgain (currentEntry model)) (UpdateEntry << MsgPasswordAgain)
    , viewValidation model
    , button [onClick Add] [text "Ok"]
    , viewStorage model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
    div [ style "color" "red" ] (List.map text (errors model))

viewStorage : Model -> Html msg
viewStorage model =
  let renderEntry entry =
        case entry of
          Build_StoredEntry nm _ -> li [] [text nm]
  in
  div []
    [ ul [] (List.map (renderEntry << proj1_sig) (proj1_sig <| users model))]

-- MAIN

main : Program () Model StorageMsg
main =
  Browser.element
  { init = \f -> initModel
  , update = updateModel
  , view = view
  , subscriptions = \_ -> Sub.none}
