module Fusion where

import Text
import Graphics.Collage as Collage
import Graphics.Element as Element

type Atom = H | H2 | He3 | He4

mass : Atom -> Float
mass a =
  case a of
    H -> 1
    H2 -> 2
    He3 -> 3
    He4 -> 4

charge : Atom -> Float
charge a =
  case a of
    H -> 1
    H2 -> 1
    He3 -> 2
    He4 -> 2

name : Atom -> String
name a =
  case a of
    H -> "H"
    H2 -> "H"
    He3 -> "He"
    He4 -> "He"

form : Atom -> Collage.Form
form a =
  let toText = Text.fromString << toString
      text height = Text.centered << Text.height height << Text.fromString
      nameHeight = 50
      massHeight = 20
      withMass m name =
        let t = text nameHeight name
        in
        Collage.group
          [ Collage.move (negate (5 + toFloat (Element.widthOf t) / 2), 20)
              (Collage.toForm
                (Text.rightAligned (Text.height massHeight (toText m))))
          , Collage.toForm t
          ]
        |> Collage.rotate (-pi/2)
      noMass = Collage.rotate (-pi/2) << Collage.toForm << text nameHeight
  in
  case a of
    H -> noMass "H"
    H2 -> withMass 2 "H"
    He3 -> withMass 3 "He"
    He4 -> noMass "He"

fuse : Atom -> Atom -> Maybe (Atom, List Atom)
fuse a1 a2 =
  case (a1, a2) of
    (H, H) -> Just (H2, [])
    (H, H2) -> Just (He3, [])
    (H2, H) -> Just (He3, [])
    (He3, He3) -> Just (He4, [H, H])
    (_, _) -> Nothing
