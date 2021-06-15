module Hack (Wag, WagM, initialWag, wag, unwag, wagb, psci, PSCIT_, FieldsM, Fields') where

import Prelude
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
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

---
instance convertPSCITVol0Pure :: ConvertOption PSCIT_ "vol0" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITVol0Identity :: ConvertOption PSCIT_ "vol0" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITVol0F :: ConvertOption PSCIT_ "vol0" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

instance convertPSCITVol1Pure :: ConvertOption PSCIT_ "vol1" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITVol1Identity :: ConvertOption PSCIT_ "vol1" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITVol1F :: ConvertOption PSCIT_ "vol1" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

instance convertPSCITVol2Pure :: ConvertOption PSCIT_ "vol2" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITVol2Identity :: ConvertOption PSCIT_ "vol2" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITVol2F :: ConvertOption PSCIT_ "vol2" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

---
instance convertPSCITDVolPure :: ConvertOption PSCIT_ "dvol" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITDVolIdentity :: ConvertOption PSCIT_ "dvol" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITDVolF :: ConvertOption PSCIT_ "dvol" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

instance convertPSCITDelayPure :: ConvertOption PSCIT_ "delay" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITDelayIdentity :: ConvertOption PSCIT_ "delay" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITDelayF :: ConvertOption PSCIT_ "delay" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

instance convertPSCITDFiltPure :: ConvertOption PSCIT_ "dfilt" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITDFiltIdentity :: ConvertOption PSCIT_ "dfilt" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITDFiltF :: ConvertOption PSCIT_ "dfilt" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

--
instance convertPSCITVocPure :: ConvertOption PSCIT_ "voc" Number (Maybe (Number -> Number)) where
  convertOption _ _ = pure <<< const

instance convertPSCITVocIdentity :: ConvertOption PSCIT_ "voc" (Maybe Number) (Maybe (Number -> Number)) where
  convertOption _ _ = map const

instance convertPSCITVocF :: ConvertOption PSCIT_ "voc" (Number -> Number) (Maybe (Number -> Number)) where
  convertOption _ _ = pure

type Fields' (a :: Type)
  = ( rate0 :: a
    , rate1 :: a
    , rate2 :: a
    , pitch0 :: a
    , pitch1 :: a
    , pitch2 :: a
    , vol0 :: a
    , vol1 :: a
    , vol2 :: a
    , dvol :: a
    , delay :: a
    , dfilt :: a
    , voc :: a
    )

type FieldsM
  = ( | Fields' (Maybe (Number -> Number)) )

type Fields
  = ( | Fields' (Number -> Number) )

defaultOptions :: { | FieldsM }
defaultOptions =
  { rate0: empty
  , rate1: empty
  , rate2: empty
  , pitch0: empty
  , pitch1: empty
  , pitch2: empty
  , vol0: empty
  , vol1: empty
  , vol2: empty
  , dvol: empty
  , delay: empty
  , dfilt: empty
  , voc: empty
  }

psci ::
  forall provided.
  ConvertOptionsWithDefaults PSCIT_ { | FieldsM } { | provided } { | FieldsM } =>
  { | provided } ->
  { | FieldsM }
psci provided = all
  where
  all :: { | FieldsM }
  all = convertOptionsWithDefaults PSCIT_ defaultOptions provided

wagTag = "__w4g__" :: String

newtype WagM
  = WagM (String /\ { | FieldsM })

newtype Wag
  = Wag (String /\ { | Fields })

instance showWag :: Show Wag where
  show _ = "Wagged!"

instance showWagM :: Show WagM where
  show _ = "Wagged!"

wag :: { | FieldsM } -> WagM
wag = WagM <<< (/\) wagTag

unwag :: Wag -> { | Fields }
unwag (Wag f) = snd f

foreign import wag_ :: WagM -> Effect WagM

initialWag :: Effect (Ref.Ref Wag)
initialWag =
  Ref.new
    ( Wag $ wagTag
        /\ { rate0: const 1.0
          , rate1: const 1.0
          , rate2: const 1.0
          , pitch0: const 220.0
          , pitch1: const 440.0
          , pitch2: const 880.0
          , vol0: const 1.0
          , vol1: const 1.0
          , vol2: const 1.0
          , dvol: const 0.0
          , dfilt: const 100.0
          , delay: const 0.2
          , voc: const 0.0
          }
    )

wagb :: Ref.Ref Wag -> Behavior Wag
wagb wagRef =
  behavior \eAB ->
    makeEvent \cont -> do
      eAB
        `subscribe`
          \aToB -> do
            (WagM (_ /\ nw)) <- wag_ (wag $ psci {})
            (Wag (_ /\ old)) <- Ref.read wagRef
            let
              mx' =
                { rate0: fromMaybe old.rate0 nw.rate0
                , rate1: fromMaybe old.rate1 nw.rate1
                , rate2: fromMaybe old.rate2 nw.rate2
                , pitch0: fromMaybe old.pitch0 nw.pitch0
                , pitch1: fromMaybe old.pitch1 nw.pitch1
                , pitch2: fromMaybe old.pitch2 nw.pitch2
                , vol0: fromMaybe old.vol0 nw.vol0
                , vol1: fromMaybe old.vol1 nw.vol1
                , vol2: fromMaybe old.vol2 nw.vol2
                , dvol: fromMaybe old.dvol nw.dvol
                , dfilt: fromMaybe old.dfilt nw.dfilt
                , delay: fromMaybe old.delay nw.delay
                , voc: fromMaybe old.voc nw.voc
                }
            let
              mx = Wag $ wagTag /\ mx'
            Ref.write mx wagRef
            cont $ aToB mx
