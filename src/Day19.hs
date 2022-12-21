module Day19
  ( day19,
  )
where

import Common
import Control.Monad
import Control.Monad.State
import Data.HashMap.Strict (HashMap, (!), (!?))
import Data.HashMap.Strict qualified as H
import Data.Hashable
import Data.Maybe
import Data.Traversable
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day19 :: AOCSolution
day19 input = show <$> ([part1, part2] <*> pure (parseInput input))
  where
    part1 = sum . zipWith (*) [1 ..] . map (solve 24)
    part2 = product . map (solve 32) . take 3

data Resource
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Bounded, Enum, Eq, Ord)

instance Show Resource where
  show Ore = "ore"
  show Clay = "clay"
  show Obsidian = "obsidian"
  show Geode = "geode"

instance Read Resource where
  readsPrec _ =
    (: []) . (,[]) . \case
      "ore" -> Ore
      "clay" -> Clay
      "obsidian" -> Obsidian
      "geode" -> Geode
      _ -> undefined

instance Hashable Resource where
  hashWithSalt s = hashWithSalt s . fromEnum

type Input = [Blueprint]

type Blueprint = HashMap Resource Cost

type Cost = HashMap Resource Int

type Robots = HashMap Resource Int

type Stock = HashMap Resource Int

none :: HashMap Resource Int
none = H.fromList [(r, 0) | r <- enumerate]

parseInput :: String -> Input
parseInput = map (parse' blueprint id) . lines
  where
    blueprint = do
      P.string "Blueprint "
      number
      P.string ": "
      H.fromList <$> robot `P.sepBy` P.char ' '
    robot = do
      P.string "Each "
      target <- resource
      P.string " robot costs "
      costList <- fmap H.fromList $ cost `P.sepBy` P.string " and "
      P.string "."
      pure (target, costList)
    cost = do
      n <- number
      P.char ' '
      r <- resource
      pure (r, n)
    resource :: Parser Resource
    resource = read <$> P.many P.letter

triangle :: Int -> Int
triangle n = n * succ n `div` 2

solve :: Int -> Blueprint -> Int
solve time bp = evalState (go time (H.insert Ore 1 none) none) 0
  where
    maxR = foldl1 (H.unionWith max) $ H.elems bp
    recipes = H.toList bp
    go :: Int -> Robots -> Stock -> State Int Int
    go t robots bag = do
      let delays = mapMaybe getDelay recipes
          wait = t * robots ! Geode + bag ! Geode
      best <- get
      if best >= wait + triangle (pred t)
        then pure 0
        else do
          paths <- traverse getPaths delays
          let r = maximum $ wait : paths
          modify $ max r
          pure r
      where
        getDelay :: (Resource, Cost) -> Maybe (Resource, Cost, Int)
        getDelay (target, cost) = do
          waitTimes <- flip H.traverseWithKey cost \resource amountNeeded ->
            case robots ! resource of
              0 -> Nothing
              r -> Just $ div (max 0 (amountNeeded - bag ! resource) + r - 1) r
          let delay = maximum waitTimes + 1
          guard $ t > delay
          when (target /= Geode) $
            guard $ (maxR ! target - robots ! target) * t > bag ! target
          pure (target, cost, delay)
        getPaths :: (Resource, Cost, Int) -> State Int Int
        getPaths (target, cost, delay) =
          go
            (t - delay)
            (H.insertWith (+) target 1 robots)
            (H.unionWith (-) (H.unionWith (+) bag $ fmap (delay *) robots) cost)
