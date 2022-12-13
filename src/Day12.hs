module Day12 (
  day12
) where

import Common
import PathFinding (findPath)
import Data.Char (ord)
import Data.Maybe ( fromJust, catMaybes )
import Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict ((!))

data NodeType = Start | End | Step Int deriving (Show, Eq)

day12 :: AOCSolution
day12 input = show <$> [p1, p2]
  where
    i = parseInput input
    g = makeGraph i
    [start, end] = (head . H.keys) . (\x -> H.filter (== x) i) <$> [Start, End]
    as = H.keys $ H.filter (== Step 0) i
    fp = findPath (fmap (1,) . (!) g) end
    p1 = fst . last . fromJust $ fp start
    p2 = minimum . map (fst . last) <$> catMaybes $ fp <$> as
  

getHeight :: H.HashMap Point2d NodeType -> Point2d -> Maybe (Point2d, Int)
getHeight i p = case H.lookup p i of
  Just Start    -> Just (p,  0)
  Just End      -> Just (p, 26)
  Just (Step x) -> Just (p,  x)
  Nothing       -> Nothing

parseInput :: String -> H.HashMap Point2d NodeType
parseInput = H.fromList .
  concat .
  zipWith f [0..] .
  map (zip [0..]) .
  map2 parseChar .
  lines
  where
    f a = map (\(b, v) -> ((a, b), v))

parseChar :: Char -> NodeType
parseChar = \case
  'S' -> Start
  'E' -> End
  x   -> Step $ ord x - ord 'a'

makeGraph :: H.HashMap Point2d NodeType -> H.HashMap Point2d [Point2d]
makeGraph i = H.fromList $ map g $ H.keys i
  where
    g p = (p, ns)
      where
        Just (_, h)  = getHeight i p
        ns = map fst . filter isReachable . catMaybes $ getHeight i <$> point2dNeighbours p
        isReachable (_, h') = (h - h') <= 1
