module Rigid where

import List

import Vector
import Vector (Vector, (*.), (.+.), (.-.))

type alias PointMass = { point : Vector, mass : Float }
type alias Body = List PointMass

square
  :  { centre : Vector, mass : Float, displacement : Float, orientation : Float }
  -> Body
square { centre, mass, displacement, orientation } =
  let at offset =
        { mass = mass / 4
        , point = centre .+. Vector.rotate orientation (displacement *. offset)
        }
  in
  [ at { x = 1, y = 1 }
  , at { x = 1, y = -1 }
  , at { x = -1, y = 1 }
  , at { x = -1, y = -1 }
  ]

totalMass : Body -> Float
totalMass = List.sum << List.map (\pm -> pm.mass)

centreOfMass : Body -> Vector
centreOfMass points =
  let tm = totalMass points
  in
  Vector.sum (List.map (\pm -> (pm.mass / tm) *. pm.point) points)

momentOfInertia : Body -> Float
momentOfInertia points =
  let cm = centreOfMass points
  in
  List.sum (List.map (\pm -> pm.mass * Vector.length (pm.point .-. cm) ^ 2) points)

applyAt
  :  { force : Vector, point : Vector }
  -> Body
  -> { accel : Vector, rotAccel : Float }
applyAt { force, point } body =
  let tm = totalMass body
      cm = centreOfMass body
      toCM = cm .-. point
      dToCM = Vector.length toCM
      unitToCM = (1 / dToCM) *. toCM
      perpToCM = Vector.dot force (Vector.rotate (turns 0.25) unitToCM)
      rotAccel = perpToCM * dToCM / momentOfInertia body
  in
  { accel = (1 / tm) *. force, rotAccel = rotAccel }

a1 *+* a2 =
  { accel = a1.accel .+. a2.accel, rotAccel = a1.rotAccel + a2.rotAccel }
accelZero = { accel = Vector.zero, rotAccel = 0 }

applyAts
  :  List { force : Vector, point : Vector }
  -> Body
  -> { accel : Vector, rotAccel : Float }
applyAts forcepoints body =
  List.foldr (\fp acc -> acc *+* applyAt fp body) accelZero forcepoints
