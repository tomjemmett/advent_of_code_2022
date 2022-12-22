{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day22
  ( day22,
  )
where

import Common
import Control.Monad.State
import Data.Bifunctor (first, second)
import Data.HashMap.Strict qualified as H
import Data.List.Split (splitOn)
import Debug.Trace (trace, traceM)
import Text.Parsec qualified as P

type Grid = H.HashMap Point2d Char

type Direction = Char

data Facing = FaceRight | FaceDown | FaceLeft | FaceUp deriving (Enum, Show)

data Instruction = Move Int | Turn Direction deriving (Show)

type Instructions = [Instruction]

type Input = (Grid, Instructions)

-- ignore the sample for part 2, the code is not generalised
day22 :: AOCSolution
day22 input = go i start <$> wrapP1 : [wrapP2 | H.size g /= 96]
  where
    i@(g, _) = parseInput input
    start = minimum $ H.keys g

go :: Input -> Point2d -> (Grid -> Wrap) -> String
go (g, i) s w = show $ evalState (move (w g) i g) (s, FaceRight)

parseInput :: String -> Input
parseInput input = (g', parseInstructions i)
  where
    [g, i] = splitDoubleNewlines input
    g' = H.fromList $ concatMap parseLine $ zip [1 ..] $ lines g
    parseLine :: (Int, String) -> [(Point2d, Char)]
    parseLine (y, v) = filter ((/= ' ') . snd) $ first (y,) <$> zip [1 ..] v
    parseInstructions :: String -> Instructions
    parseInstructions = parse' p id
      where
        pn = Move <$> number
        pt = Turn <$> P.oneOf "LR"
        p = P.many $ P.choice [pn, pt]

move :: Wrap -> Instructions -> Grid -> State (Point2d, Facing) Int
move _ [] g = do
  ((y, x), f) <- get
  pure (x * 4 + y * 1000 + fromEnum f)
move w (i : is) g = do
  modify $ flip move' i
  move w is g
  where
    move' (p, f) = \case
      Move x -> m p f x
      Turn x -> (p, t x f)
    -- helper function to move from point p in facing f n positions
    m :: Point2d -> Facing -> Int -> (Point2d, Facing)
    m p f 0 = (p, f)
    m p f n = case v of
      Just '.' -> m p' f (pred n)
      Just '#' -> (p, f)
      Nothing ->
        let (p'', f') = w p f
         in if g H.! p'' == '#' then (p, f) else m p'' f' (pred n)
      where
        p' = d f p
        v = H.lookup p' g
    -- helper function to turn
    t 'R' = \case
      FaceRight -> FaceDown
      FaceDown -> FaceLeft
      FaceLeft -> FaceUp
      FaceUp -> FaceRight
    t 'L' = \case
      FaceRight -> FaceUp
      FaceUp -> FaceLeft
      FaceLeft -> FaceDown
      FaceDown -> FaceRight
    d = \case
      FaceRight -> second succ
      FaceDown -> first succ
      FaceLeft -> second pred
      FaceUp -> first pred

-- helper function to wrap if we reach end of grid
type Wrap = Point2d -> Facing -> (Point2d, Facing)

wrapP1 :: Grid -> Wrap
wrapP1 g (y, x) f = case f of
  FaceRight -> (minimum $ filter ((== y) . fst) $ H.keys g, f)
  FaceDown -> (minimum $ filter ((== x) . snd) $ H.keys g, f)
  FaceLeft -> (maximum $ filter ((== y) . fst) $ H.keys g, f)
  FaceUp -> (maximum $ filter ((== x) . snd) $ H.keys g, f)

wrapP2 :: Grid -> Wrap
wrapP2 g (y, x) = \case
  -- this method is only called when we get a Nothing, so it's safe not to do full bounds checks
  FaceRight
    -- box A
    | x == 50 -> ((150, y - 100), FaceUp)
    -- box C
    | x == 100 && y > 100 -> ((151 - y, 150), FaceLeft)
    --box D
    | x == 100 -> ((50, y + 50), FaceUp)
    -- box F
    | x == 150 -> ((151 - y, 100), FaceLeft)
  FaceDown
    -- box A
    | y == 200 -> ((1, x + 100), FaceDown)
    -- box C
    | y == 150 -> ((x + 100, 50), FaceLeft)
    -- box F
    | y == 50 -> ((x - 50, 100), FaceLeft)
  FaceLeft
    -- box A
    | x == 1 && y > 150 -> ((1, y - 100), FaceDown)
    -- box B
    | x == 1 -> ((151 - y, 51), FaceRight)
    -- box D
    | x == 51 && y > 50 -> ((101, y - 50), FaceDown)
    -- box E
    | x == 51 -> ((151 - y, 1), FaceRight)
  FaceUp
    -- box B
    | y == 101 -> ((x + 50, 51), FaceRight)
    -- box E
    | y == 1 && x <= 100 -> ((x + 100, 1), FaceRight)
    -- box F
    | y == 1 -> ((200, x - 100), FaceUp)
