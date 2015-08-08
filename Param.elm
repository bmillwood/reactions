module Param where
import Time

fps = 20
repulsion = 160000 / Time.second^2
maxPushRepulsion = repulsion / 10
maxRotRepulsion = repulsion / 100
dragCoef = 0.0001
fastTurn = 3 / Time.second
playerAccel = 0.2 / Time.second
playerTurnSpeed = 0.04 / Time.second
otherRotDecel = 0.1 / Time.second
minFusionDistance = 30
minTimeBetweenFuses = 1 * Time.second
