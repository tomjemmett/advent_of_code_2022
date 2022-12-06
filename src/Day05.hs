module Day05 (
  day05
) where

import Common
import Data.Maybe (catMaybes)
import Data.List (group)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Text.Parsec as P
import Text.Parsec ((<|>))

type Move = (Int, Int, Int)
type State = M.Map Int String

day05 :: AOCSolution
day05 input = go <$> [reverse, id] <*> parseInput input

go :: (String -> String) -> (State, [Move]) -> String
go f = map (head . snd) . M.toList . uncurry (foldl (move f))

move :: (String -> String) -> State -> Move -> State
move f m0 (n, x, y) = m2 $ m1 $ m0
  where
    (a, b) = splitAt n $ m0 M.! x
    m1 = M.insertWith (++) y (f a)
    m2 = M.insert x b

parseInput :: Applicative f => String -> f (State, [Move])
parseInput = pure . parse' p id
  where
    p = do
      rows <- parseCrates
      P.anyChar `P.manyTill` P.newline -- ignore the numbers line
      P.newline
      moves <- parseMoves
      pure (rows, moves)

parseCrates = M.fromListWith (++) . reverse . concat <$> pRows
  where
    pRows  =  pRow `P.sepBy` P.newline
    pRow   = catMaybes . zipWith (\x -> fmap (x,)) [1..] <$> pCrate `P.sepBy` P.char ' '
    pCrate = pEmpty <|> pFull
    pEmpty = P.try $ const Nothing <$> P.string "   "
    pFull  = P.char '[' *> (Just . (:[]) <$> P.letter) <* P.char ']'

parseMoves = pMove `P.sepEndBy` P.newline
  where
    -- parse the moves
    pMove = do 
      P.string "move "
      n <- number
      P.string " from "
      x <- number
      P.string " to "
      y <- number
      return (n, x, y)