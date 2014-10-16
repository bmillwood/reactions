import Mouse
import Window

h : Form
h = toForm <| plainText "H"

draw : Signal Form -> Signal Element
draw sf =
  let k (x,y) f = collage x y [f]
  in k <~ Window.dimensions ~ sf

adjustedPosition : Signal (Float, Float)
adjustedPosition =
  let k (x,y) (w,h) = (toFloat x - toFloat w/2, toFloat h/2 - toFloat y)
  in k <~ Mouse.position ~ Window.dimensions

main : Signal Element
main = draw (lift (\pos -> move pos h) adjustedPosition)
