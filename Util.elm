module Util where
-- Util: things that ought to be in the standard library, but aren't

unfoldr : (b -> Maybe (a, b)) -> b -> [a]
unfoldr k seed =
  case k seed of
    Nothing -> []
    Just (x, new) -> x :: unfoldr k new

contexts : [a] -> [([a], a, [a])]
contexts xs =
  case xs of
    [] -> []
    x :: xs ->
      let go (before, here, after) =
            case after of
              [] -> Nothing
              next :: rest ->
                let ctx = (here :: before, next, rest)
                in Just (ctx, ctx)
      in ([], x, xs) :: unfoldr go ([], x, xs)

