import Char
import Dict
import Graphics.Collage
import Keyboard
import List
import Mouse
import Signal
import Text
import Window

data ThingId = ID
type Vector = { x : Float, y : Float }
type RelVector = { forward : Float, right : Float }
type Dynamics = { position : Vector, motion : Vector, orientation : Float }
type World = Dict.Dict ThingId Dynamics
type Control = { move : RelVector, turn : Float }

(.+.) : Vector -> Vector -> Vector
a .+. b = { x = a.x + b.x, y = a.y + b.y }

key : Char -> Signal Bool
key = Keyboard.isDown << Char.toCode

control : Signal Control
control =
  let posNeg pos neg =
        if | pos && not neg -> 1 | neg && not pos -> -1 | otherwise -> 0
      keyPair posChar negChar = posNeg <~ key posChar ~ key negChar
      keysControl ws ad eq =
        { move = { forward = 0.5 * ws, right = 0.5 * eq }
        , turn = 0.1 * ad
        }
  in
  keysControl <~ keyPair 'w' 's' ~ keyPair 'a' 'd' ~ keyPair 'e' 'q'
  |> Signal.sampleOn (fps 10)

motionVector : Float -> RelVector -> Vector
motionVector orient { forward, right } =
  { x = forward * cos orient + right * sin orient
  , y = forward * sin orient - right * cos orient
  }

bounce : Dynamics -> Dynamics
bounce dyn =
  let newX =
        if | dyn.position.x >= 200 -> negate (abs dyn.motion.x)
           | dyn.position.x <= -200 -> abs dyn.motion.x
           | otherwise -> dyn.motion.x
      newY =
        if | dyn.position.y >= 200 -> negate (abs dyn.motion.y)
           | dyn.position.y <= -200 -> abs dyn.motion.y
           | otherwise -> dyn.motion.y
  in
  { dyn | motion <- { x = newX, y = newY } }

stepDynamics : Control -> Dynamics -> Dynamics
stepDynamics control dynamics =
  let acceleration = motionVector dynamics.orientation control.move
  in
  { position = dynamics.position .+. dynamics.motion
  , motion = dynamics.motion .+. acceleration
  , orientation = dynamics.orientation + control.turn
  } |> bounce

thingDynamics : Signal Dynamics
thingDynamics =
  Signal.foldp
    stepDynamics
    { position = { x = 0, y = 0 }, motion = { x = 0, y = 0 }
    , orientation = pi/2
    }
    control

avatar : Form
avatar = rotate (-pi/2) << toForm << centered << Text.height 50 <| toText "H"

main : Signal Element
main =
  let scene (w,h) dyn con =
        let pos = dyn.position
            canvas = collage w h [move (pos.x, pos.y) (rotate dyn.orientation avatar)]
            debug = flow down [asText con, asText dyn]
        in
        debug `above` canvas
  in
  scene <~ Window.dimensions ~ thingDynamics ~ control
