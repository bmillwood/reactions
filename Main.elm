module Main where

import Char
import Graphics.Collage
import Keyboard
import List
import Mouse
import Signal
import Text
import Time
import Window

import Param
import Vector
import Vector (Vector, (.+.), (.-.), (*.))

type RelVector = { forward : Float, right : Float }
type Momentum = { pos : Vector, ang : Float }
type Dynamics = { now : Momentum, change : Momentum }
type Control = { move : RelVector, turn : Float }

key : Char -> Signal Bool
key = Keyboard.isDown << Char.toCode

ticks : Signal Float
ticks = Time.inSeconds <~ fps Param.fps

control : Signal Control
control =
  let posNeg pos neg =
        if | pos && not neg -> 1 | neg && not pos -> -1 | otherwise -> 0
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

type Thing = { avatar : Form, dyn : Dynamics }
data World = W Thing [Thing]

h : Form
h = rotate (-pi/2) << toForm << centered << Text.height 50 <| toText "H"

firstWorld : World
firstWorld =
  let player =
        { avatar = h
        , dyn =
          { now = { pos = Vector.zero, ang = pi/2 }
          , change = { pos = Vector.zero, ang = 0 }
          }
        }
      other =
        { avatar = h
        , dyn =
          { now = { pos = { x = 100, y = 0 }, ang = pi/2 }
          , change = { pos = { x = 0.3, y = 0.2 }, ang = 1 }
          }
        }
  in
  W player [other]

unfoldr : (b -> Maybe (a, b)) -> b -> [a]
unfoldr k seed =
  case k seed of
    Nothing -> []
    Just (x, new) -> x :: unfoldr k new

contexts : [a] -> [([a], a, [a])]
contexts xs =
  case xs of
    [] -> []
    x :: xs ->
      let go (before, here, after) =
            case after of
              [] -> Nothing
              next :: rest ->
                let ctx = (here :: before, next, rest)
                in Just (ctx, ctx)
      in ([], x, xs) :: unfoldr go ([], x, xs)

stepWorld : (Control, Time, (Int, Int)) -> World -> World
stepWorld (ctl, timeDelta, (w, h)) (W player others) =
  let pushBetween x y =
        let displacement = y.dyn.now.pos .-. x.dyn.now.pos
            distance = Vector.length displacement
        in (negate Param.gravity / distance^3) *. displacement
      pushOn x ys = foldl (.+.) Vector.zero (map (pushBetween x) ys)
      newPlayer =
        let push = pushOn player others
            accel = Param.playerAccel *. motionVector player.dyn.now.ang ctl.move .+. push
            turn = Param.playerTurnSpeed * ctl.turn
        in 
        { avatar = player.avatar
        , dyn = stepDynamics timeDelta accel turn (w, h) player.dyn
        }
      newOther (before, other, after) =
        let push = pushBetween other player
              .+. pushOn other before
              .+. pushOn other after
        in
        { avatar = other.avatar
        , dyn = stepDynamics timeDelta push 0.1 (w, h) other.dyn
        }
  in
  W newPlayer (map newOther (contexts others))

world : Signal World
world = foldp stepWorld firstWorld
  ((,,) <~ sampleOn ticks control ~ ticks ~ Window.dimensions)

main : Signal Element
main =
  let drawWorld (w,h) (W player others) con delta =
        let drawThing thing =
              move (thing.dyn.now.pos.x, thing.dyn.now.pos.y)
                (rotate thing.dyn.now.ang thing.avatar)
            canvas = collage w h (map drawThing (player :: others))
            debug = flow down (asText delta :: asText con :: asText player :: map asText others)
        in
        layers [canvas, debug]
  in
  drawWorld <~ Window.dimensions ~ world ~ control ~ ticks
