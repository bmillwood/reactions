module Util where
-- Util: things that ought to be in the standard library, but aren't

unfoldr : (b -> Maybe (a, b)) -> b -> List a
unfoldr k seed =
  case k seed of
    Nothing -> []
    Just (x, new) -> x :: unfoldr k new

contexts : List a -> List (List a, a, List a)
contexts xs =
  let go (before, after) =
        case after of
          [] -> Nothing
          next :: rest -> Just ((before, next, rest), (next :: before, rest))
  in
  unfoldr go ([], xs)
