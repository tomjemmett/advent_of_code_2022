module Day16
  ( day16,
  )
where

import Common
import Data.Bifunctor (first)
import Data.List (foldl', maximumBy)
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Tree qualified as T
import PathFinding
import Text.Parsec ((<|>))
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day16 :: AOCSolution
day16 input = show <$> ([part1, part2] <*> parseInput input)

parseInput :: Applicative f => String -> f [(String, (Int, [String]))]
parseInput = pure . map (parse' p id) . lines
  where
    p = do
      v <- P.string "Valve " *> P.many1 P.letter
      r <- P.string " has flow rate=" *> number
      P.try (P.string "; tunnels lead to valves ")
        <|> P.string "; tunnel leads to valve "
      vs <- P.many P.letter `P.sepBy` P.string ", "
      pure (v, (r, vs))

getVertices :: [(String, (Int, [String]))] -> M.Map String Int
getVertices = M.filter (> 0) . M.map fst . M.fromList

getEdges :: [(String, (Int, [String]))] -> M.Map String [(Int, String)]
getEdges = M.map (map (1,) . snd) . M.fromList

distanceTable :: [(String, (Int, [String]))] -> M.Map String (M.Map String Int)
distanceTable i = M.fromList do
  let v = M.keys $ getVertices i
      e = getEdges i
      f = (M.!) e
  k1 <- "AA" : v
  let p = [(k2,) $ fst $ last $ unsafeFindPath f k1 k2 | k2 <- v, k1 /= k2]
  pure (k1, M.fromList p)

stateTree :: Int -> [(String, (Int, [String]))] -> M.Map String Int -> T.Tree (String, Int)
stateTree maxT i valves = T.unfoldTree buildNode (1, start, M.empty)
  where
    dists = distanceTable i
    start = "AA"
    nextStates (t, v, zs)
      | t >= maxT = []
      | v /= start && v `M.notMember` zs = [(t + 1, v, M.insert v ((maxT - t) * valves M.! v) zs)]
      | otherwise =
        [ (t + dists M.! v M.! w, w, zs)
          | w <- M.keys valves,
            w `M.notMember` zs
        ]
    buildNode (t, v, zs) = ((v, if null succs then foldl' (+) 0 zs else 0), succs)
      where
        succs = nextStates (t, v, zs)

leaves :: T.Tree (a, b) -> [([a], b)]
leaves (T.Node (n, v) []) = [([n], v)]
leaves (T.Node (n, _) xs) = map (first (n :)) $ concatMap leaves xs

part1 :: [(String, (Int, [String]))] -> Int
part1 i = maximum . map snd . leaves $ tr
  where
    tr = stateTree 30 i $ getVertices i

part2 :: [(String, (Int, [String]))] -> Int
part2 i = f1 + f2
  where
    vs1 = getVertices i
    (p, f1) = maximumBy (comparing snd) . leaves $ stateTree 26 i vs1
    vs2 = vs1 `M.withoutKeys` S.fromList p
    (_, f2) = maximumBy (comparing snd) . leaves $ stateTree 26 i vs2
