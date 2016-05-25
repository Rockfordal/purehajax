module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Free (liftFI)

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (AJAX(), post, get)
import Data.JSON
import Data.Tuple
import Data.Map as M

-- | The state of the component.
type State = { busy :: Boolean,
               user :: String,
               payload :: String,
               result :: Maybe String }

initialState :: State
initialState = { busy: false,
                 payload: (userRequest "Jack"),
                 user: "Jack",
                 result: Nothing }

userRequest :: String -> String
userRequest name =
  (encode jsonData)
  where
  jsonData = Tuple "useridkey" name


firebaseUrl =  "https://samplechat.firebaseio-demo.com/message_list.json"

-- | The component query algebra.
data Query a
  = SetPayload String a
  | SetUser String a
  | MakeRequest String a

-- | The effects used in the app.
type AppEffects eff = HalogenEffects (ajax :: AJAX | eff)

-- | The definition for the app's main UI component.
ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render st =
    H.div_ $
      [ H.h1_
          [ H.text "Halogen Ajax" ]
      , H.h2_
          [ H.text "Meddelande:" ]
      , H.input
          [ P.id_ "msgdata"
          --, P.classes [ B.formControl ]
          --, P.inputType inpType
          , P.value st.user
          , E.onValueInput (E.input SetUser)
          ]
      , H.p_
          [ H.text st.user ]
{-      , H.p_
          [ H.textarea
              [ P.value st.payload
              , E.onValueInput (E.input SetPayload)]] -}
      , H.p_
          [ H.button
              [ P.disabled st.busy
              , E.onClick (E.input_ (MakeRequest st.payload))
              ]
              [ H.text "Skicka" ]
          ]
      , H.p_
          [ H.text (if st.busy then "Working..." else "") ]
      ]
      ++ flip foldMap st.result \js ->
          [ H.div_
              [ H.h2_
                  [ H.text "Svar:" ]
              , H.pre_
                  [ H.code_ [ H.text js ] ]]]

  eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
  eval (SetPayload payload next) = modify (_ { payload = payload, result = Nothing :: Maybe String }) $> next
  eval (SetUser user next) = modify (_ { user = user }) $> next
  eval (MakeRequest payload next) = do
    modify (_ { busy = true })
    result <- liftAff' (fetchJS payload)
    modify (_ { busy = false,
                result = Just result })
    pure next

-- | Post some PureScript code to the trypurescript API and fetch the JS result.
fetchJS :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchJS payload = do
  result <- post firebaseUrl payload
  let response = result.response
  return response
  --return case readProp "js" response <|> readProp "error" response of
  --  Right js -> js
  --  Left _ -> "Invalid response"

-- | Run the app.
--main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
