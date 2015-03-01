module Main where

import Char
import Graphics.Collage as Collage
import Graphics.Element (Element)
import Keyboard
import List
import Mouse
import Random
import Signal
import Signal (Signal, (<~), (~))
import Text
import Time
import Time (Time)
import Window

import Fusion
import Param
import Util (contexts)
import Vector
import Vector (Vector, (.+.), (.-.), (*.))

type alias RelVector = { forward : Float, right : Float }
type alias Momentum = { pos : Vector, ang : Float }
type alias Dynamics = { now : Momentum, change : Momentum }
type alias Control = { move : RelVector, turn : Float }

ticks : Signal Time
ticks = Time.fps Param.fps

control : Signal Control
control =
  let posNeg pos neg =
        if | pos && not neg -> 1 | neg && not pos -> -1 | otherwise -> 0
      key = Keyboard.isDown << Char.toCode
      keyPair posChar negChar = posNeg <~ key posChar ~ key negChar
      keysControl ws ad eq =
        { move = { forward = ws, right = eq }
        , turn = ad
        }
  in
  keysControl <~ keyPair 'w' 's' ~ keyPair 'a' 'd' ~ keyPair 'e' 'q'

motionVector : Float -> RelVector -> Vector
motionVector orient { forward, right } =
  Vector.rotate orient { x = right, y = forward }

bounce : Int -> Int -> Dynamics -> Dynamics
bounce w h dyn =
  let wf = toFloat w / 2
      hf = toFloat h / 2
      change = dyn.change
  in
  { dyn | change <-
    { change | pos <-
      { x = 
        if | dyn.now.pos.x >= wf -> negate (abs dyn.change.pos.x)
           | dyn.now.pos.x <= -wf -> abs dyn.change.pos.x
           | otherwise -> dyn.change.pos.x
      , y =
        if | dyn.now.pos.y >= hf -> negate (abs dyn.change.pos.y)
           | dyn.now.pos.y <= -hf -> abs dyn.change.pos.y
           | otherwise -> dyn.change.pos.y
      }
    }
  }

stepDynamics : Time -> Vector -> Float -> (Int, Int) -> Dynamics -> Dynamics
stepDynamics timeDelta accel turn (w, h) dyn =
  let drag =
        let v = dyn.change.pos
            len = Vector.length v
            magnitude = Param.dragCoef * len^2
            force = (magnitude / len) *. Vector.negate v
        in
        if abs len < 0.1 then Vector.zero else force
  in
  { now =
    { pos = dyn.now.pos .+. timeDelta *. dyn.change.pos .+. timeDelta^2 *. accel
    , ang = dyn.now.ang + timeDelta * dyn.change.ang
    }
  , change =
    { pos = dyn.change.pos .+. timeDelta *. (accel .+. drag)
    , ang = turn
    }
  } |> bounce w h

type alias Thing = { atom : Fusion.Atom, noFuseTime : Time, dyn : Dynamics }
type World = W Thing (List Thing)

firstWorld : World
firstWorld =
  let hAt p =
        { atom = Fusion.H
        , noFuseTime = 0
        , dyn =
          { now = { pos = p, ang = 0 }
          , change = { pos = Vector.zero, ang = 0 }
          }
        }
  in
  W (hAt Vector.zero)
    [ hAt { x = -100, y = -50 }
    , hAt { x = 100, y = 0 }
    , hAt { x = 130, y = 80 }
    , hAt { x = 260, y = 160 }
    , hAt { x = -110, y = 10 }
    , hAt { x = -210, y = 30 }
    , hAt { x = -220, y = -70 }
    , hAt { x = -20, y = -270 }
    , hAt { x = 20, y = 250 }
    ]

fuse : World -> World
fuse world =
  let tryFuse other (W player others) =
        let shouldFuse =
              other.noFuseTime == 0
                && player.noFuseTime == 0
                && Vector.length (player.dyn.now.pos .-. other.dyn.now.pos)
                    <= Param.minFusionDistance
        in
        case (shouldFuse, Fusion.fuse player.atom other.atom) of
          (True, Just (newPlayerAtom, newOthers)) ->
            let thingMomentum thing =
                  Fusion.mass thing.atom *. thing.dyn.change.pos
                thingAngMom thing =
                  Fusion.mass thing.atom * thing.dyn.change.ang
                fusedMomentum = thingMomentum player .+. thingMomentum other
                fusedDirection = Vector.direction fusedMomentum
                fusedAngMom = thingAngMom player + thingAngMom other
                fusedMass = List.sum (List.map Fusion.mass (newPlayerAtom :: newOthers))
                fusedAng =
                  Vector.direction <|
                    Vector.unit player.dyn.now.ang
                    .+. Vector.unit other.dyn.now.ang
                fusedMove = (1 / fusedMass) *. fusedMomentum
                fusedPosition = 0.5 *. (player.dyn.now.pos .+. other.dyn.now.pos)
                numNew = 1 + List.length newOthers
                makeNew atom newAngle displacement =
                  { atom = atom
                  , noFuseTime = Param.minTimeBetweenFuses
                  , dyn =
                      { now =
                        { pos = fusedPosition .+. displacement
                        , ang = newAngle
                        }
                      , change = { pos = fusedMove, ang = 0 }
                      }
                  }
                newPlayer =
                  makeNew newPlayerAtom player.dyn.now.ang
                    (10 *. Vector.unit fusedDirection)
                newOther ix otherAtom =
                  makeNew otherAtom fusedAng
                    (10 *. Vector.unit (2 * pi * toFloat (ix + 1) / toFloat numNew))
            in
            W newPlayer (List.indexedMap newOther newOthers ++ others)
          (_, _) -> W player (other :: others)
  in
  case world of
    W player [] -> W player []
    W player (other :: others) -> case fuse (W other others) of
      W other others -> List.foldr tryFuse (W player []) (other :: others)

doPhysics : Control -> Time -> (Int, Int) -> World -> World
doPhysics ctl timeDelta (w, h) (W player others) =
  let pushBetween x y =
        let displacement = y.dyn.now.pos .-. x.dyn.now.pos
            distance = Vector.length displacement
            rawMagnitude =
              Fusion.charge x.atom * Fusion.charge y.atom
                * Param.repulsion / distance^2
            clippedMagnitude = min rawMagnitude Param.maxRepulsion
        in (negate clippedMagnitude / distance) *. displacement
      pushOn x ys = Vector.sum (List.map (pushBetween x) ys)
      newPlayer =
        let push = pushOn player others
            accel =
              Param.playerAccel *. motionVector player.dyn.now.ang ctl.move
                .+. (1 / Fusion.mass player.atom) *. push
            turn = Param.playerTurnSpeed * ctl.turn
        in 
        { atom = player.atom
        , noFuseTime = max 0 (player.noFuseTime - timeDelta)
        , dyn = stepDynamics timeDelta accel turn (w, h) player.dyn
        }
      newOther (before, other, after) =
        let push = pushBetween other player
              .+. pushOn other before
              .+. pushOn other after
            accel = (1 / Fusion.mass other.atom) *. push
        in
        { atom = other.atom
        , noFuseTime = max 0 (other.noFuseTime - timeDelta)
        , dyn = stepDynamics timeDelta accel 0 (w, h) other.dyn
        }
  in
  W newPlayer (List.map newOther (contexts others))

stepWorld : (Control, Time, (Int, Int)) -> World -> World
stepWorld (ctl, timeDelta, (w, h)) =
  fuse << doPhysics ctl timeDelta (w, h)

world : Signal World
world = Signal.foldp stepWorld firstWorld
  ((,,) <~ Signal.sampleOn ticks control ~ ticks ~ Window.dimensions)

main : Signal Element
main =
  let drawWorld (w,h) (W player others) con delta =
        let drawThing thing =
              Collage.move (thing.dyn.now.pos.x, thing.dyn.now.pos.y)
                (Collage.rotate thing.dyn.now.ang (Fusion.form thing.atom))
        in
        Collage.collage w h (List.map drawThing (player :: others))
  in
  drawWorld <~ Window.dimensions ~ world ~ control ~ ticks
