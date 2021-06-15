module Audio where

import Prelude
import Control.Apply.Indexed ((:*>))
import Control.Comonad.Cofree (Cofree, head, tail)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
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
import WAGS.Graph.AudioUnit (OnOff(..), TGain, TPeriodicOsc, TSpeaker)
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
    , mix :: TGain /\ { unit0 :: Unit, unit1 :: Unit, unit2 :: Unit }
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
    , r0p :: Number
    , r1p :: Number
    , r2p :: Number
    , p0p :: Number
    , p1p :: Number
    , p2p :: Number
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
            , r0p: 1.0
            , r1p: 1.0
            , r2p: 1.0
            , p0p: 220.0
            , p1p: 440.0
            , p2p: 880.0
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
    @!> iloop \e { asdr0, asdr1, asdr2, ptime, u0ph, u1ph, u2ph, r0p, r1p, r2p, p0p, p1p, p2p } ->
        let
          { time, headroom, world } = e

          uw = unwag world time

          w =
            { rate0: fromMaybe r0p uw.rate0
            , rate1: fromMaybe r1p uw.rate1
            , rate2: fromMaybe r2p uw.rate2
            , pitch0: fromMaybe p0p uw.pitch0
            , pitch1: fromMaybe p1p uw.pitch1
            , pitch2: fromMaybe p2p uw.pitch2
            }

          mp = makePulse headroom (time - ptime)

          res = { u0: mp asdr0 u0ph w.rate0, u1: mp asdr1 u1ph w.rate1, u2: mp asdr2 u2ph w.rate2 }
        in
          ichange
            { unit0: head $ snd res.u0
            , unit1: head $ snd res.u1
            , unit2: head $ snd res.u2
            , osc0: w.pitch0
            , osc1: w.pitch1
            , osc2: w.pitch2
            }
            $> { asdr0: tail $ snd res.u0
              , asdr1: tail $ snd res.u1
              , asdr2: tail $ snd res.u2
              , ptime: time
              , u0ph: fst res.u0
              , u1ph: fst res.u1
              , u2ph: fst res.u2
              , r0p: w.rate0
              , r1p: w.rate1
              , r2p: w.rate2
              , p0p: w.pitch0
              , p1p: w.pitch1
              , p2p: w.pitch2
              }
