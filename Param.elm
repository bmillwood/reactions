module Param where
import Time

fps = 20
repulsion = 160000 / Time.second^2
maxRepulsion = repulsion / 10
dragCoef = 0.0001
fastTurn = 3 / Time.second
playerAccel = 0.2 / Time.second
playerTurnSpeed = 0.04 / Time.second
minFusionDistance = 30
minTimeBetweenFuses = 1 * Time.second
