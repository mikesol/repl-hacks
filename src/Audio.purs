module Audio where

import Prelude

import Control.Apply.Indexed ((:*>))
import Control.Comonad.Cofree (Cofree, head, tail)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D4, D5)
import Data.Vec ((+>))
import Data.Vec as V
import Hack (Wag, unwag)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (OnOff(..), TDelay, TGain, THighpass, TPeriodicOsc, TSpeaker)
import WAGS.Graph.Parameter (AudioParameter, ff)
import WAGS.Interpret (class AudioInterpret)
import WAGS.NE2CF (ASDR, TimeHeadroom, makeLoopingPiecewise)
import WAGS.Patch (ipatch)
import WAGS.Run (SceneI)

type POsc (a :: Type)
  = V.Vec a Number /\ V.Vec a Number

osc0 :: POsc D4
osc0 = (0.0 +> 0.2 +> -0.1 +> 0.05 +> V.empty) /\ (0.0 +> 0.02 +> 0.03 +> 0.1 +> V.empty)

osc1 :: POsc D4
osc1 = (0.0 +> 0.03 +> 0.05 +> 0.1 +> V.empty) /\ (0.0 +> 0.1 +> -0.2 +> 0.03 +> V.empty)

osc2 :: POsc D5
osc2 = (0.0 +> 0.01 +> -0.2 +> -0.1 +> 0.05 +> V.empty) /\ (0.0 +> 0.01 +> 0.02 +> 0.2 +> 0.01 +> V.empty)

type FrameTp a e p i o x
  = IxWAG a e p Unit i o x

startAgain = 0.11 :: Number

pwf :: NonEmpty List (Number /\ Number)
pwf = (0.00 /\ 0.0) :| (0.03 /\ 1.0) : (0.07 /\ 0.1) : (0.09 /\ 0.0) : Nil

lpwf = makeLoopingPiecewise startAgain pwf :: ASDR

type SceneType
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { unit0 :: Unit, unit1 :: Unit, unit2 :: Unit, del :: Unit }
    , del :: TDelay /\ { dmix :: Unit }
    , dmix :: TDelay /\ { dhpf :: Unit }
    , dhpf :: THighpass /\ { mix :: Unit }
    , unit0 :: TGain /\ { osc0 :: Unit }
    , osc0 :: TPeriodicOsc /\ {}
    , unit1 :: TGain /\ { osc1 :: Unit }
    , osc1 :: TPeriodicOsc /\ {}
    , unit2 :: TGain /\ { osc2 :: Unit }
    , osc2 :: TPeriodicOsc /\ {}
    }

type Acc
  = { asdr0 :: ASDR
    , asdr1 :: ASDR
    , asdr2 :: ASDR
    , ptime :: Number
    , u0ph :: Number
    , u1ph :: Number
    , u2ph :: Number
    }

type Extern
  = SceneI Unit Wag

createFrame ::
  forall audio engine.
  AudioInterpret audio engine =>
  Extern -> FrameTp audio engine Frame0 {} SceneType Acc
createFrame { time } =
  ipatch
    :*> ( ichange
          { mix: 0.1
          , unit0: 0.0
          , unit1: 0.0
          , unit2: 0.0
          , osc0: { waveform: osc0, freq: 220.0, onOff: On }
          , osc1: { waveform: osc1, freq: 440.0, onOff: On }
          , osc2: { waveform: osc2, freq: 880.0, onOff: On }
          }
          $> { asdr0: (map <<< map) (ff 0.06) lpwf
            , asdr1: (map <<< map) (ff 0.06) lpwf
            , asdr2: (map <<< map) (ff 0.06) lpwf
            , ptime: time
            , u0ph: 0.0
            , u1ph: 0.0
            , u2ph: 0.0
            }
      )

type CF
  = Cofree ((->) TimeHeadroom) (AudioParameter)

makePulse :: Int -> Number -> ASDR -> Number -> Number -> Number /\ CF
makePulse headroom tdiff asdr tnow rate =
  let
    tn = tnow + (tdiff * rate)

    cf = asdr { time: tn, headroom: toNumber headroom / 1000.0 }
  in
    tn /\ cf

piece ::
  forall audio engine.
  AudioInterpret audio engine =>
  Scene Extern audio engine Frame0 Unit
piece =
  createFrame
    @!> iloop \e { asdr0, asdr1, asdr2, ptime, u0ph, u1ph, u2ph } ->
        let
          { time, headroom, world } = e

          w = unwag world

          mp = makePulse headroom (time - ptime)

          res =
            { u0: mp asdr0 u0ph (w.rate0 time)
            , u1: mp asdr1 u1ph (w.rate1 time)
            , u2: mp asdr2 u2ph (w.rate2 time)
            }
        in
          ichange
            { unit0: head (snd res.u0) * pure (w.vol0 time)
            , unit1: head (snd res.u1) * pure (w.vol1 time)
            , unit2: head (snd res.u2) * pure (w.vol2 time)
            , osc0: w.pitch0 time
            , osc1: w.pitch1 time
            , osc2: w.pitch2 time
            , dhpf: w.dfilt time
            , del: w.delay time
            , dmix: w.dvol time
            }
            $> { asdr0: tail $ snd res.u0
              , asdr1: tail $ snd res.u1
              , asdr2: tail $ snd res.u2
              , ptime: time
              , u0ph: fst res.u0
              , u1ph: fst res.u1
              , u2ph: fst res.u2
              }
