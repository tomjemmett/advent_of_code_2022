module Day14 (
  day14
) where

import Common
import Control.Applicative ((<|>))
import Data.HashMap.Strict qualified as H
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

data Input = Input
  { inputGrid :: H.HashMap Point2d Char
  , mapBottom :: Int
  }

data SimulationState = SimulationState
  { gridState     :: H.HashMap Point2d Char
  , threshold     :: Int
  , currentGrain  :: Point2d
  , settledGrains :: Int
  }

parseInput :: String -> Input
parseInput = createInput . concatMap parseLine . lines

createInput :: [Point2d] -> Input
createInput points = Input
  { inputGrid = H.fromList $ map (,'#') points
  , mapBottom = maximum $ map snd points
  }

parseLine :: String -> [(Int, Int)]
parseLine i = concatMap coordsToLines $ zip l (tail l)
  where
    l = parse' (p `P.sepBy` P.string " -> ") id i
    coordsToLines ((x1, y1), (x2, y2))
      | x1 <= x2 && y1 <= y2 = [(a,b) | a <- [x1..x2], b <- [y1..y2]]
      | x1 <= x2             = [(a,b) | a <- [x1..x2], b <- [y2..y1]]
      |             y1 <= y2 = [(a,b) | a <- [x2..x1], b <- [y1..y2]]
      | otherwise            = [(a,b) | a <- [x2..x1], b <- [y2..y1]]
    p :: Parser (Int, Int)
    p = do
      a <- number
      P.char ','
      b <- number
      pure (a, b)

day14 :: AOCSolution
day14 input = go <$> [part1, part2] <*> pure (parseInput input)
  where
    part1 s@SimulationState {..} = snd currentGrain > threshold
    part2 s@SimulationState {..} = H.member currentGrain gridState

startPoint :: Point2d
startPoint = (500, 0)

initState :: Input -> SimulationState
initState Input {..} = SimulationState
  { gridState = inputGrid
  , threshold = mapBottom
  , currentGrain = startPoint
  , settledGrains = 0
  }

instance Show SimulationState where
  show i = unlines x
    where
      g = H.insert (currentGrain i) 'x' (gridState i)
      k = H.keys g
      minX = minimum $ fst <$> k
      maxX = maximum $ fst <$> k
      minY = 0
      maxY = (+1) $ maximum $ snd <$> k
      x = [ [ H.findWithDefault ' ' (x, y) g | x <- [minX..maxX] ]
          | y <- [minY..maxY]
          ]

moveGrain :: H.HashMap Point2d Char -> Point2d -> Maybe Point2d
moveGrain grid p@(x, y) = down <|> downLeft <|> downRight
  where
    available p = if H.member p grid then Nothing else Just p
    down      = available (x, succ y)
    downLeft  = available (pred x, succ y)
    downRight = available (succ x, succ y)

step :: (SimulationState -> Bool) -> SimulationState -> Maybe SimulationState
step target s@SimulationState {..}
  | target s = Nothing
  | snd currentGrain == threshold + 1 = reset
  | otherwise = case moveGrain gridState currentGrain of
      Just p -> Just $ s { currentGrain = p }
      Nothing      -> reset
  where
    reset = Just $ s
      { currentGrain  = startPoint
      , gridState     = H.insert currentGrain 'o' gridState
      , settledGrains = succ settledGrains
      }

go :: (SimulationState -> Bool) -> Input -> String
go f = show . settledGrains . run . initState
  where
    run s = maybe s run $ step f s