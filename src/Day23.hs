{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day23
  ( day23,
  )
where

import Common
import Data.HashSet qualified as S
import Data.Map qualified as M
import Data.Maybe (catMaybes)

type Elves = S.HashSet Point2d

day23 :: AOCSolution
day23 input = show <$> ([part1, part2 1 directions] <*> pure (parseInput input))

directions :: [Char]
directions = cycle "NSWE"

parseInput :: String -> Elves
parseInput = S.fromList . map fst . filter ((/= '.') . snd) . prepare2dGrid

prepare2dGrid :: String -> [(Point2d, Char)]
prepare2dGrid input =
  [ ((x, y), c)
    | (y, r) <- zip [0 ..] $ lines input,
      (x, c) <- zip [0 ..] r
  ]

part1 :: Elves -> Int
part1 i = area - S.size i
  where
    i' = foldl (flip goRound) i (take 10 directions)
    ((minx, maxx), (miny, maxy)) = getBounds $ S.toList i'
    area = (maxx - minx + 1) * (maxy - miny + 1)

part2 :: Int -> [Char] -> Elves -> Int
part2 n (d : ds) i = if i == i' then n else part2 (succ n) ds i'
  where
    i' = goRound d i

goRound :: Char -> Elves -> Elves
goRound d elves = secondHalf elves $ firstHalf d elves

firstHalf :: Char -> Elves -> [Maybe Point2d]
firstHalf d elves = map (makeProposal d elves) $ S.toList elves

secondHalf :: Elves -> [Maybe Point2d] -> Elves
secondHalf elves proposals = S.fromList $ px <$> zip i proposals
  where
    i = S.toList elves
    pc = countReduce $ catMaybes proposals
    px (x, Nothing) = x
    px (x, Just p) = if pc M.! p > 1 then x else p

makeProposal :: Char -> Elves -> Point2d -> Maybe Point2d
makeProposal d elves e =
  if and unoccupied
    then Nothing
    else case d of
      'N' -> g $ p 0
      'S' -> g $ p 1
      'W' -> g $ p 2
      'E' -> g $ p 3
  where
    neighbours = point2dNeighboursDiags e
    unoccupied = map (not . (`S.member` elves)) neighbours
    f is@(i : _) = (neighbours !! i, all (unoccupied !!) is)
    p n =
      take 4 $
        drop n $
          cycle
            [ f [2, 4, 6],
              f [3, 5, 7],
              f [0, 4, 5],
              f [1, 6, 7]
            ]
    g [] = Nothing
    g ((m, t) : xs) = if t then Just m else g xs

getBounds :: [Point2d] -> ((Int, Int), (Int, Int))
getBounds i = ((minx, maxx), (miny, maxy))
  where
    minx = minimum $ map fst i
    maxx = maximum $ map fst i
    miny = minimum $ map snd i
    maxy = maximum $ map snd i
