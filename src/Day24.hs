module Day24
  ( day24,
  )
where

import Common
import Control.Monad (guard)
import Data.HashMap.Strict qualified as H
import Data.HashSet qualified as S

day24 :: AOCSolution
day24 input = show <$> [t1, t3]
  where
    i = parseInput input
    (t1, j) = bfs i (end i) (0, S.singleton $ start i)
    (t2, k) = bfs j (start i) (t1, S.singleton $ end i)
    (t3, _) = bfs k (end i) (t2, S.singleton $ start i)

data Grid = Grid
  { start :: Point2d,
    end :: Point2d,
    maxx :: Int,
    maxy :: Int,
    blizzards :: Blizzards
  }

instance Show Grid where
  show Grid {..} =
    unlines $
      ['#' : '.' : replicate maxx '#']
        ++ ['#' : debugLine y ++ "#" | y <- [0 .. (pred maxy)]]
        ++ [replicate maxx '#' ++ ".#"]
    where
      debugLine y = do
        x <- [0 .. (pred maxx)]
        let p = (x, y)
            v = H.lookup p blizzards
        pure $ case v of
          Nothing -> '.'
          Just [x] -> head $ show x
          Just xs -> head $ show $ length xs

data Blizzard = BLeft | BRight | BUp | BDown

instance Read Blizzard where
  readsPrec _ =
    (: []) . (,[]) . \case
      ">" -> BRight
      "<" -> BLeft
      "^" -> BUp
      "v" -> BDown
      _ -> undefined

instance Show Blizzard where
  show = \case
    BRight -> ">"
    BLeft -> "<"
    BUp -> "^"
    BDown -> "v"

type Blizzards = H.HashMap Point2d [Blizzard]

parseInput :: String -> Grid
parseInput input = Grid start end maxx maxy g'
  where
    g = do
      -- start at -1, the edges get dropped giving us 0,0 as first used coordinate
      (y, l) <- zip [-1 ..] $ lines input
      (x, c) <- zip [-1 ..] l
      pure ((x, y), c)
    g' = H.map ((: []) . read . (: [])) $ H.filter (`elem` "><^v") $ H.fromList g
    (maxx, maxy) = maximum $ map fst g
    dots = map fst $ filter ((== '.') . snd) g
    start = (0, -1)
    end = head $ filter ((== maxy) . snd) dots

moveBlizzards :: Grid -> Grid
moveBlizzards g@Grid {..} = g {blizzards = blizzards'}
  where
    blizzards' = foldr (uncurry (H.insertWith (++)) . move) H.empty (concatMap f $ H.toList blizzards)
    f (a, bs) = zip (repeat a) bs
    move ((x, y), b) = (,[b]) $ case b of
      BRight -> (succ x `mod` maxx, y)
      BLeft -> ((x + pred maxx) `mod` maxx, y)
      BUp -> (x, (y + pred maxy) `mod` maxy)
      BDown -> (x, succ y `mod` maxy)

moveExpedition :: Grid -> Point2d -> Point2d -> [Point2d]
moveExpedition Grid {..} goal pos = do
  p@(x, y) <- pos : point2dNeighbours pos
  guard $ p == pos || p == goal || (y >= 0 && y < maxy)
  guard $ x >= 0 && x < maxx
  guard $ not $ H.member p blizzards
  pure p

bfs :: Grid -> Point2d -> (Int, S.HashSet Point2d) -> (Int, Grid)
bfs g goal (n, pos) =
  if S.member goal pos
    then (n, g)
    else bfs g' goal (succ n, nxt)
  where
    g' = moveBlizzards g
    nxt = S.fromList $ concatMap (moveExpedition g' goal) pos