module Main where

import Char
import Graphics.Collage as Collage
import Graphics.Element (Element)
import Keyboard
import List
import Mouse
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

ticks : Signal Float
ticks = Time.inSeconds <~ Time.fps Param.fps

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
  { x = forward * cos orient + right * sin orient
  , y = forward * sin orient - right * cos orient
  }

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
  { now =
      { pos = dyn.now.pos .+. timeDelta *. dyn.change.pos .+. timeDelta^2 *. accel
      , ang = dyn.now.ang + timeDelta * dyn.change.ang
      }
  , change = { pos = dyn.change.pos .+. timeDelta *. accel, ang = turn }
  } |> bounce w h

type alias Thing = { atom : Fusion.Atom, dyn : Dynamics }
type World = W Thing (List Thing)

firstWorld : World
firstWorld =
  let player =
        { atom = Fusion.H
        , dyn =
          { now = { pos = Vector.zero, ang = pi/2 }
          , change = { pos = Vector.zero, ang = 0 }
          }
        }
  in
  W player
    [ { atom = Fusion.H
      , dyn =
        { now = { pos = { x = 100, y = 0 }, ang = pi/2 }
        , change = { pos = { x = 0.3, y = 0.2 }, ang = 1 }
        }
      }
    , { atom = Fusion.H
      , dyn =
        { now = { pos = { x = 130, y = 80 }, ang = pi/2 }
        , change = { pos = { x = -1, y = 0 }, ang = -1 }
        }
      }
    , { atom = Fusion.H
      , dyn =
        { now = { pos = { x = 80, y = 180 }, ang = pi/2 }
        , change = { pos = { x = 0, y = 2 }, ang = -1 }
        }
      }
    , { atom = Fusion.H
      , dyn =
        { now = { pos = { x = -50, y = 80 }, ang = pi/2 }
        , change = { pos = { x = -1, y = 1 }, ang = -1 }
        }
      }
    , { atom = Fusion.H
      , dyn =
        { now = { pos = { x = -130, y = -80 }, ang = pi/2 }
        , change = { pos = { x = 0, y = 0 }, ang = -1 }
        }
      }
    ]

fuse : World -> World
fuse world =
  let tryFuse other (W player others) =
        let d = Vector.length (player.dyn.now.pos .-. other.dyn.now.pos)
        in
        case (d < Param.minFusionDistance, Fusion.fuse player.atom other.atom) of
          (True, Just (newPlayerAtom, newOthers)) ->
            let thingMomentum thing =
                  Fusion.mass thing.atom *. thing.dyn.change.pos
                thingAngMom thing =
                  Fusion.mass thing.atom * thing.dyn.change.ang
                totalMomentum = thingMomentum player .+. thingMomentum other
                totalAngMom = thingAngMom player + thingAngMom other
                totalMass = List.sum (List.map Fusion.mass (newPlayerAtom :: newOthers))
                newPlayerMass = Fusion.mass newPlayerAtom
                playerFrac = newPlayerMass / totalMass
                newPlayer =
                  { atom = newPlayerAtom
                  , dyn =
                      { now = player.dyn.now
                      , change =
                        { pos = (playerFrac / newPlayerMass) *. totalMomentum
                        , ang = 0 }
                      }
                  }
            in
            W newPlayer others -- TODO: deal with newOthers
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
            magnitude =
              Fusion.charge x.atom * Fusion.charge y.atom
                * Param.repulsion / distance^2
        in (negate magnitude / distance) *. displacement
      pushOn x ys = Vector.sum (List.map (pushBetween x) ys)
      newPlayer =
        let push = pushOn player others
            accel =
              Param.playerAccel *. motionVector player.dyn.now.ang ctl.move
                .+. (1 / Fusion.mass player.atom) *. push
            turn = Param.playerTurnSpeed * ctl.turn
        in 
        { atom = player.atom
        , dyn = stepDynamics timeDelta accel turn (w, h) player.dyn
        }
      newOther (before, other, after) =
        let push = pushBetween other player
              .+. pushOn other before
              .+. pushOn other after
            accel = (1 / Fusion.mass other.atom) *. push
        in
        { atom = other.atom
        , dyn = stepDynamics timeDelta accel 0.1 (w, h) other.dyn
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
