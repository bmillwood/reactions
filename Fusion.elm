module Fusion where

import List
import Text
import Graphics.Collage as Collage
import Graphics.Element as Element

type Atom = H | H2 | He3 | He4 | Be7

mass : Atom -> Float
mass a =
  case a of
    H -> 1
    H2 -> 2
    He3 -> 3
    He4 -> 4
    Be7 -> 7

charge : Atom -> Float
charge a =
  case a of
    H -> 1
    H2 -> 1
    He3 -> 2
    He4 -> 2
    Be7 -> 3

name : Atom -> String
name a =
  case a of
    H -> "H"
    H2 -> "H"
    He3 -> "He"
    He4 -> "He"
    Be7 -> "Be"

textH : Float -> String -> Element.Element
textH height = Text.centered << Text.height height << Text.fromString

elementH : Float -> Atom -> Element.Element
elementH height a =
  let mk = textH height
  in
  case a of
    H -> mk "H"
    H2 -> mk "²H"
    He3 -> mk "³He"
    He4 -> mk "He"
    Be7 -> mk "⁷Be"

element : Atom -> Element.Element
element = elementH 45

form : Atom -> Collage.Form
form = Collage.toForm << element

fuse : Atom -> Atom -> Maybe (Atom, List Atom)
fuse a1 a2 =
  case (a1, a2) of
    (H, H) -> Just (H2, [])
    (H, H2) -> Just (He3, [])
    (H2, H) -> Just (He3, [])
    (He3, He3) -> Just (He4, [H, H])
    (He3, He4) -> Just (Be7, [])
    (He4, He3) -> Just (Be7, [])
    (_, _) -> Nothing

help : Atom -> Element.Element
help a =
  let sp = Element.spacer 10 30
      plus x xs = sp :: textH 30 "+" :: sp :: elementH 30 x :: xs
      sum xs =
        Element.flow Element.right
          (List.drop 3 (List.foldr plus [] xs))
      mk lhs rhs = Element.flow Element.right [sp, sum lhs, sp, textH 30 "→", sp, sum rhs]
  in
  case a of
    H -> mk [H,H] [H2]
    H2 -> mk [H2,H] [He3]
    He3 -> mk [He3,He3] [He4,H,H]
    He4 -> mk [He4,He3] [Be7]
    Be7 -> Element.empty
