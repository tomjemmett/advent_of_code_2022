module Day10 (
  day10
) where

import Common
import Control.Monad.State (MonadState(put, get), runState, State)
import Data.List.Split (chunksOf)

day10 :: AOCSolution
day10 input = [show p1, unlines $ init $ chunksOf 40 ('#':p2)]
  where
    (p2, (_, _, p1)) = runState (traverse go $ words input) (0, 1, 0)

-- go takes the next instruction word and modifies a state containing
-- * cycle number
-- * x register value
-- * the current sum for part 1
-- it returns whether the pixel is lit or not in that cycle
go :: String -> State (Int, Int, Int) Char
go = \case
  "noop"      -> f 0
  "addx"      -> f 0 
  (read -> a) -> f a
  where
    f a = do
      -- get the current state:
      -- * increment the counter
      -- * update the x register by adding in a
      -- * get the current value for part 1
      (succ -> c, (+a) -> x, p) <- get
      let
        cm = c `mod` 40
        -- update the current value for part 2
        p' = p + if cm == 19 then succ c * x else 0
      -- update the state
      put (c, x, p')
      -- return the pixel for this cycle
      pure $ if abs (cm - x) <= 1 then '#' else '.'
