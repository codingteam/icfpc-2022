module Util where

-- | Dummy function that should behave the same as Math.round() in JavaScript.
--
-- Right now, we only round positive inputs, so this doesn't matter much.
jsRound :: Integral a => Double -> a
jsRound x
  | x <= 0 = error "jsRound is not implemented for non-positive numbers"
  | otherwise = round x
