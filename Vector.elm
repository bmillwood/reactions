module Vector where

type Vector = { x : Float, y : Float }

zero : Vector
zero = { x = 0, y = 0 }

(.+.) : Vector -> Vector -> Vector
a .+. b = { x = a.x + b.x, y = a.y + b.y }

(.-.) : Vector -> Vector -> Vector
a .-. b = { x = a.x - b.x, y = a.y - b.y }

length : Vector -> Float
length { x, y } = sqrt (x^2 + y^2)

map : (Float -> Float) -> Vector -> Vector
map f v = { x = f v.x, y = f v.y }

(*.) : Float -> Vector -> Vector
(*.) k = map ((*) k)

negate : Vector -> Vector
negate = (*.) (-1)
