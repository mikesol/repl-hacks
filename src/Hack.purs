module Hack(Wag, wag, unwag, wagb, psci, PSCIT_, PSCIT, All, Optional) where

import Prelude

import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Maybe (Maybe)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import FRP.Behavior (Behavior, behavior)
import FRP.Event (makeEvent, subscribe)

data PSCIT_
  = PSCIT_

instance convertPSCITRate0Pure :: ConvertOption PSCIT_ "rate0" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITRate0Identity :: ConvertOption PSCIT_ "rate0" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITRate0F :: ConvertOption PSCIT_ "rate0" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

instance convertPSCITRate1Pure :: ConvertOption PSCIT_ "rate1" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITRate1Identity :: ConvertOption PSCIT_ "rate1" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITRate1F :: ConvertOption PSCIT_ "rate1" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

instance convertPSCITRate2Pure :: ConvertOption PSCIT_ "rate2" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITRate2Identity :: ConvertOption PSCIT_ "rate2" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITRate2F :: ConvertOption PSCIT_ "rate2" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure
---
instance convertPSCITPitch0Pure :: ConvertOption PSCIT_ "pitch0" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITPitch0Identity :: ConvertOption PSCIT_ "pitch0" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITPitch0F :: ConvertOption PSCIT_ "pitch0" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

instance convertPSCITPitch1Pure :: ConvertOption PSCIT_ "pitch1" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITPitch1Identity :: ConvertOption PSCIT_ "pitch1" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITPitch1F :: ConvertOption PSCIT_ "pitch1" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

instance convertPSCITPitch2Pure :: ConvertOption PSCIT_ "pitch2" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITPitch2Identity :: ConvertOption PSCIT_ "pitch2" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITPitch2F :: ConvertOption PSCIT_ "pitch2" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

type Optional :: forall k. k -> Row k
type Optional a
  = ( rate0 :: a, rate1 :: a, rate2 :: a, pitch0 :: a, pitch1 :: a, pitch2 :: a )

type All
  = ( | Optional (Maybe (Number -> Number)) )

defaultOptions :: { | All }
defaultOptions =
  { rate0:  empty
  , rate1:  empty
  , rate2:  empty
  , pitch0:  empty
  , pitch1:  empty
  , pitch2:  empty
  }

psci ::
  forall provided.
  ConvertOptionsWithDefaults PSCIT_ { | All } { | provided } { | All } =>
  { | provided } ->
  { | All }
psci provided = all
  where
  all :: { | All }
  all = convertOptionsWithDefaults PSCIT_ defaultOptions provided

type PSCIT
  = { | All }

wagTag = "__w4g__" :: String

newtype Wag = Wag (String /\ PSCIT)

instance showWag :: Show Wag where
  show _ = "Wagged!"

wag :: PSCIT -> Wag
wag = Wag <<< (/\) wagTag

unwag :: Wag -> PSCIT
unwag (Wag f) = snd f

foreign import wag_ :: Wag -> Effect Wag

wagb :: Behavior Wag
wagb = behavior \eAB ->
  makeEvent \cont -> do
     eAB `subscribe` \aToB -> wag_ (wag $ psci {}) >>= cont <<< aToB