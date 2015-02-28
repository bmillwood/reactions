module Param where
import Time

fps = 20
repulsion = 160000 / Time.second^2
maxRepulsion = 1000000 / Time.second^2
playerAccel = 0.2 / Time.second
playerTurnSpeed = 2.5 / Time.second
minFusionDistance = 30
minTimeBetweenFuses = 1 * Time.second
