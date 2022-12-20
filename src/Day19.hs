module Day19
  ( day19,
  )
where

import Common
import Text.Parsec ((<|>))
import Text.Parsec qualified as P
import Text.Parsec.Text (Parser)

type Resources = [Int]

type Robots = [Int]

type Blueprint = [Resources]

type State = (Resources, Robots)

testInput =
  "Blueprint 1: \
  \Each ore robot costs 4 ore. \
  \Each clay robot costs 2 ore. \
  \Each obsidian robot costs 3 ore and 14 clay. \
  \Each geode robot costs 2 ore and 7 obsidian. \n\
  \\
  \Blueprint 2: \
  \Each ore robot costs 2 ore. \
  \Each clay robot costs 3 ore. \
  \Each obsidian robot costs 3 ore and 8 clay. \
  \Each geode robot costs 3 ore and 12 obsidian."

i = parseInput testInput

bps = head i

maxR = init $ foldl1 (zipWith max) bps

day19 :: AOCSolution
day19 input = show <$> [p1, p2]
  where
    i = parseInput input
    p1 = sum $ zipWith (*) [1 ..] $ map (solve 24) i
    p2 = product $ map (solve 32) $ take 3 i

parseInput :: String -> [Blueprint]
parseInput = map (parse' p id) . lines
  where
    p = do
      P.string "Blueprint " *> number
      a1 <- P.string ": Each ore robot costs " *> number
      b1 <- P.string " ore. Each clay robot costs " *> number
      c1 <- P.string " ore. Each obsidian robot costs " *> number
      c2 <- P.string " ore and " *> number
      d1 <- P.string " clay. Each geode robot costs " *> number
      d3 <- P.string " ore and " *> number
      P.string " obsidian."
      pure
        [ [a1, 0, 0, 0],
          [b1, 0, 0, 0],
          [c1, c2, 0, 0],
          [d1, 0, d3, 0]
        ]

solve :: Int -> Blueprint -> Int
solve t bps = go t 0 ([0, 0, 0, 0], [1, 0, 0, 0])
  where
    maxR = init $ foldl1 (zipWith max) bps
    go :: Int -> Int -> State -> Int
    -- we have reached the end time, return either the current best, or the number of geodes produced
    go 0 !best (last -> !ge, _) = max best ge
    go t !best (!rcs, !rbs)
      -- if this state can't beat the current best, don't attempt it
      | (t * pred t `div` 2) + last rcs + last rbs * t <= best = best
      -- if we can produce the geode robot, just do that
      | last cp = go (pred t) best $ last nx
      -- dfs next states
      | otherwise = foldl (go (pred t)) best ns
      where
        -- do we have enough resources to produce using a given blueprint?
        cp = map (and . zipWith (>=) rcs) bps
        -- should we manufacture this robot, or do we already have enough?
        cm = zipWith (&&) cp $ zipWith (>=) maxR rcs
        -- get the next ore/clay/obsidian resources/robots
        nx =
          zip
            (zipWith (+) rbs . zipWith (-) rcs <$> bps)
            (zipWith (+) rbs <$> [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]])
        -- filter nx based on cm, and prepend the "null" option of just producing resources
        ns = (zipWith (+) rbs rcs, rbs) : map snd (filter fst $ zip cm nx)