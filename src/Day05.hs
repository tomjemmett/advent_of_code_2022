module Day05 (
  day05
) where

import Common
import Data.List (group)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Text.Parsec as P

type Move = (Int, Int, Int)
type State = M.Map Int String

day05 :: AOCSolution
day05 input = go <$> [reverse, id] <*> parseInput input

go :: (String -> String) -> (State, [Move]) -> String
go f = map (head . snd) . M.toList . uncurry (foldl (move f))

parseInput :: Applicative f => String -> f (State, [Move])
parseInput input = pure (parseState state, parseMoves moves)
  where
    [state, moves] = map lines $ splitOn "\n\n" input

parseState :: [String] -> State
parseState = M.fromListWith (++) .
  -- flip the input as our data structure is a stack
  reverse .
  -- run f to parse each line, it will figure out what character is at a position
  concatMap (f 1) .
  -- the last line just contains numbers, we don't need this line
  init
  where
    -- parse 3 characters, if middle character is a space just run next
    -- otherwise, append the item to insert into the map
    f n (_:x:_:xs) = case x of
      ' ' -> next
      otherwise -> (n, [x]):next
      where
        next = g (succ n) xs
    -- if the next character is a space, continue processing
    g n (' ':xs) = f n xs 
    -- if the input is empty, stop
    g _ [] = []

parseMoves :: [String] -> [Move]
parseMoves = map (parse' p id)
  where
    p = do
      P.string "move "
      n <- number
      P.string " from "
      x <- number
      P.string " to "
      y <- number
      return (n, x, y)

move :: (String -> String) -> State -> Move -> State
move f m0 (n, x, y) = m2 $ m1 $ m0
  where
    (a, b) = splitAt n $ m0 M.! x
    m1 = M.insertWith (++) y (f a)
    m2 = M.insert x b
