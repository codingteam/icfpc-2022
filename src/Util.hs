module Util where

-- | Dummy function that should behave the same as Math.round() in JavaScript.
--
-- Right now, we only round non-negative inputs, so this doesn't matter much.
jsRound :: Integral a => Double -> a
jsRound x
  | x < 0 = error "jsRound is not implemented for negative numbers"
  | otherwise = round x
