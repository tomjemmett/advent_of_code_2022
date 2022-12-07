module Day05
  ( day05,
    parseCrates,
    parseMoves,
  )
where

import Common
import Control.Monad.State
import Data.List (group)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

type Move = (Int, Int, Int)

type Crates = M.Map Int String

day05 :: AOCSolution
day05 input = go <$> [reverse, id] <*> parseInput input

go :: (String -> String) -> (Crates, [Move]) -> String
go f (s, m) = map (head . snd) . M.toList . execState t $ s
  where
    t = traverse (move f) m

move :: (String -> String) -> Move -> State Crates ()
move f (n, x, y) = push y =<< pop f n x
  where
    pop :: (String -> String) -> Int -> Int -> State Crates String
    pop f n x = do
      s <- get
      let (a, b) = splitAt n $ s M.! x
      put $ M.insert x b s
      pure $ f a
    push :: Int -> String -> State Crates ()
    push y v = modify $ M.insertWith (++) y v

parseInput :: Applicative f => String -> f (Crates, [Move])
parseInput = pure . parse' p id
  where
    p = do
      rows <- parseCrates
      P.anyChar `P.manyTill` P.newline -- ignore the numbers line
      P.newline
      moves <- parseMoves
      pure (rows, moves)

parseCrates :: Parser Crates
parseCrates = M.fromListWith (++) . reverse . concat <$> pRows
  where
    pRows = pRow `P.sepBy` P.newline
    pRow = catMaybes . zipWith (\x -> fmap (x,)) [1 ..] <$> pCrate `P.sepBy` P.char ' '
    pCrate = pEmpty <|> pFull
    pEmpty = P.try $ Nothing <$ P.string "   "
    pFull = P.char '[' *> (Just . (: []) <$> P.letter) <* P.char ']'

parseMoves :: Parser [Move]
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